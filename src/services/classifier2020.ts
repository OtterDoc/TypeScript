import {
    BindingElement,
    CancellationToken,
    Classifications,
    ClassifiedSpan2020,
    createTextSpan,
    Debug,
    Declaration,
    EndOfLineState,
    forEachChild,
    getCombinedModifierFlags,
    getCombinedNodeFlags,
    getMeaningFromLocation,
    isBindingElement,
    isCallExpression,
    isCatchClause,
    isFunctionDeclaration,
    isIdentifier,
    isImportClause,
    isImportSpecifier,
    isInfinityOrNaNString,
    isJsxElement,
    isJsxExpression,
    isJsxSelfClosingElement,
    isNamespaceImport,
    isPropertyAccessExpression,
    isQualifiedName,
    isSourceFile,
    isVariableDeclaration,
    ModifierFlags,
    NamedDeclaration,
    Node,
    NodeFlags,
    ParameterDeclaration,
    Program,
    SemanticMeaning,
    SourceFile,
    Symbol,
    SymbolFlags,
    SyntaxKind,
    TextSpan,
    textSpanIntersectsWith,
    Type,
    TypeChecker,
    VariableDeclaration,
} from "./_namespaces/ts";

/** @internal */
export const enum TokenEncodingConsts {
    typeOffset = 8,
    modifierMask = (1 << typeOffset) - 1
}

/** @internal */
export const enum TokenType {
    class, enum, interface, namespace, typeParameter, type, parameter, variable, enumMember, property, function, member
}

/** @internal */
export const enum TokenModifier {
    declaration, static, async, readonly, defaultLibrary, local
}

/**
 * Returns an array of classified spans for a given program, source file, and text span.
 *
 * @param {Program} program - The TypeScript program.
 * @param {CancellationToken} cancellationToken - The cancellation token.
 * @param {SourceFile} sourceFile - The source file to classify.
 * @param {TextSpan} span - The text span to classify.
 * @returns {ClassifiedSpan2020[]} An array of classified spans.
 */
export function getSemanticClassifications(program: Program, cancellationToken: CancellationToken, sourceFile: SourceFile, span: TextSpan): ClassifiedSpan2020[] {
    const classifications = getEncodedSemanticClassifications(program, cancellationToken, sourceFile, span);

    Debug.assert(classifications.spans.length % 3 === 0);
    const dense = classifications.spans;
    const result: ClassifiedSpan2020[] = [];
    for (let i = 0; i < dense.length; i += 3) {
        result.push({
            textSpan: createTextSpan(dense[i], dense[i + 1]),
            classificationType: dense[i + 2]
        });
    }

    return result;
}

/** @internal */
export function getEncodedSemanticClassifications(program: Program, cancellationToken: CancellationToken, sourceFile: SourceFile, span: TextSpan): Classifications {
    return {
        spans: getSemanticTokens(program, sourceFile, span, cancellationToken),
        endOfLineState: EndOfLineState.None
    };
}

/**
 * Retrieves semantic tokens for a given program, source file, text span, and cancellation token.
 * @param program - The program to retrieve tokens from.
 * @param sourceFile - The source file to retrieve tokens from.
 * @param span - The text span to retrieve tokens from.
 * @param cancellationToken - The cancellation token to use.
 * @returns An array of numbers representing the semantic tokens.
 */
function getSemanticTokens(program: Program, sourceFile: SourceFile, span: TextSpan, cancellationToken: CancellationToken): number[] {
    const resultTokens: number[] = [];

    const collector = (node: Node, typeIdx: number, modifierSet: number) => {
        resultTokens.push(node.getStart(sourceFile), node.getWidth(sourceFile), ((typeIdx + 1) << TokenEncodingConsts.typeOffset) + modifierSet);
    };

    if (program && sourceFile) {
        collectTokens(program, sourceFile, span, collector, cancellationToken);
    }
    return resultTokens;
}

/**
 * Collects tokens from a given program, source file, and text span using a provided collector function.
 * @param program - The program to collect tokens from.
 * @param sourceFile - The source file to collect tokens from.
 * @param span - The text span to collect tokens from.
 * @param collector - The function to use for collecting tokens.
 * @param cancellationToken - The cancellation token to use for cancelling the operation.
 */
function collectTokens(program: Program, sourceFile: SourceFile, span: TextSpan, collector: (node: Node, tokenType: number, tokenModifier: number) => void, cancellationToken: CancellationToken) {
    const typeChecker = program.getTypeChecker();

    let inJSXElement = false;

    /**
     * Visits a node and classifies its symbol based on its type and modifiers.
     * @param node The node to visit.
     */
    function visit(node: Node) {
        switch(node.kind) {
            case SyntaxKind.ModuleDeclaration:
            case SyntaxKind.ClassDeclaration:
            case SyntaxKind.InterfaceDeclaration:
            case SyntaxKind.FunctionDeclaration:
            case SyntaxKind.ClassExpression:
            case SyntaxKind.FunctionExpression:
            case SyntaxKind.ArrowFunction:
                cancellationToken.throwIfCancellationRequested();
        }

        if (!node || !textSpanIntersectsWith(span, node.pos, node.getFullWidth()) || node.getFullWidth() === 0) {
            return;
        }
        const prevInJSXElement = inJSXElement;
        if (isJsxElement(node) || isJsxSelfClosingElement(node)) {
            inJSXElement = true;
        }
        if (isJsxExpression(node)) {
            inJSXElement = false;
        }

        if (isIdentifier(node) && !inJSXElement && !inImportClause(node) && !isInfinityOrNaNString(node.escapedText)) {
            let symbol = typeChecker.getSymbolAtLocation(node);
            if (symbol) {
                if (symbol.flags & SymbolFlags.Alias) {
                    symbol = typeChecker.getAliasedSymbol(symbol);
                }
                let typeIdx = classifySymbol(symbol, getMeaningFromLocation(node));
                if (typeIdx !== undefined) {
                    let modifierSet = 0;
                    if (node.parent) {
                        const parentIsDeclaration = (isBindingElement(node.parent) || tokenFromDeclarationMapping.get(node.parent.kind) === typeIdx);
                        if (parentIsDeclaration && (node.parent as NamedDeclaration).name === node) {
                            modifierSet = 1 << TokenModifier.declaration;
                        }
                    }

                    // property declaration in constructor
                    if (typeIdx === TokenType.parameter && isRightSideOfQualifiedNameOrPropertyAccess(node)) {
                        typeIdx = TokenType.property;
                    }

                    typeIdx = reclassifyByType(typeChecker, node, typeIdx);

                    const decl = symbol.valueDeclaration;
                    if (decl) {
                        const modifiers = getCombinedModifierFlags(decl);
                        const nodeFlags = getCombinedNodeFlags(decl);
                        if (modifiers & ModifierFlags.Static) {
                            modifierSet |= 1 << TokenModifier.static;
                        }
                        if (modifiers & ModifierFlags.Async) {
                            modifierSet |= 1 << TokenModifier.async;
                        }
                        if (typeIdx !== TokenType.class && typeIdx !== TokenType.interface) {
                            if ((modifiers & ModifierFlags.Readonly) || (nodeFlags & NodeFlags.Const) || (symbol.getFlags() & SymbolFlags.EnumMember)) {
                                modifierSet |= 1 << TokenModifier.readonly;
                            }
                        }
                        if ((typeIdx === TokenType.variable || typeIdx === TokenType.function) && isLocalDeclaration(decl, sourceFile)) {
                            modifierSet |= 1 << TokenModifier.local;
                        }
                        if (program.isSourceFileDefaultLibrary(decl.getSourceFile())) {
                            modifierSet |= 1 << TokenModifier.defaultLibrary;
                        }
                    }
                    else if (symbol.declarations && symbol.declarations.some(d => program.isSourceFileDefaultLibrary(d.getSourceFile()))) {
                        modifierSet |= 1 << TokenModifier.defaultLibrary;
                    }

                    collector(node, typeIdx, modifierSet);

                }
            }
        }
        forEachChild(node, visit);

        inJSXElement = prevInJSXElement;
    }
    visit(sourceFile);
}

/**
 * Determines the token type of a given symbol based on its flags and meaning.
 * @param symbol - The symbol to classify.
 * @param meaning - The semantic meaning of the symbol.
 * @returns The token type of the symbol, or undefined if it cannot be classified.
 */
function classifySymbol(symbol: Symbol, meaning: SemanticMeaning): TokenType | undefined {
    const flags = symbol.getFlags();
    if (flags & SymbolFlags.Class) {
        return TokenType.class;
    }
    else if (flags & SymbolFlags.Enum) {
        return TokenType.enum;
    }
     else if (flags & SymbolFlags.TypeAlias) {
        return TokenType.type;
    }
    else if (flags & SymbolFlags.Interface) {
        if (meaning & SemanticMeaning.Type) {
            return TokenType.interface;
        }
    }
    else if (flags & SymbolFlags.TypeParameter) {
        return TokenType.typeParameter;
    }
    let decl = symbol.valueDeclaration || symbol.declarations && symbol.declarations[0];
    if (decl && isBindingElement(decl)) {
        decl = getDeclarationForBindingElement(decl);
    }
    return decl && tokenFromDeclarationMapping.get(decl.kind);
}

/**
 * Reclassifies a token type based on the type of the node and its type checker.
 * @param {TypeChecker} typeChecker - The type checker to use.
 * @param {Node} node - The node to check.
 * @param {TokenType} typeIdx - The token type index to reclassify.
 * @returns {TokenType} - The reclassified token type.
 */
function reclassifyByType(typeChecker: TypeChecker, node: Node, typeIdx: TokenType): TokenType {
    // type based classifications
    if (typeIdx === TokenType.variable || typeIdx === TokenType.property || typeIdx === TokenType.parameter) {
        const type = typeChecker.getTypeAtLocation(node);
        if (type) {
            const test = (condition: (type: Type) => boolean) => {
                return condition(type) || type.isUnion() && type.types.some(condition);
            };
            if (typeIdx !== TokenType.parameter && test(t => t.getConstructSignatures().length > 0)) {
                return TokenType.class;
            }
            if (test(t => t.getCallSignatures().length > 0) && !test(t => t.getProperties().length > 0) || isExpressionInCallExpression(node)) {
                return typeIdx === TokenType.property ? TokenType.member : TokenType.function;
            }
        }
    }
    return typeIdx;
}

/**
 * Determines if a given declaration is a local declaration in a specific source file.
 * @param decl - The declaration to check.
 * @param sourceFile - The source file to check against.
 * @returns {boolean} - True if the declaration is local to the source file, false otherwise.
 */
function isLocalDeclaration(decl: Declaration, sourceFile: SourceFile): boolean {
    if (isBindingElement(decl)) {
        decl = getDeclarationForBindingElement(decl);
    }
    if (isVariableDeclaration(decl)) {
        return (!isSourceFile(decl.parent.parent.parent) || isCatchClause(decl.parent)) && decl.getSourceFile() === sourceFile;
    }
    else if (isFunctionDeclaration(decl)) {
        return !isSourceFile(decl.parent) && decl.getSourceFile() === sourceFile;
    }
    return false;
}

/**
 * Returns the VariableDeclaration or ParameterDeclaration for a given BindingElement.
 * @param element - The BindingElement to get the declaration for.
 * @returns The VariableDeclaration or ParameterDeclaration for the given BindingElement.
 */
function getDeclarationForBindingElement(element: BindingElement): VariableDeclaration | ParameterDeclaration {
    while (true) {
        if (isBindingElement(element.parent.parent)) {
            element = element.parent.parent;
        }
        else {
            return element.parent.parent;
        }
    }
}

function inImportClause(node: Node): boolean {
    const parent = node.parent;
    return parent && (isImportClause(parent) || isImportSpecifier(parent) || isNamespaceImport(parent));
}

function isExpressionInCallExpression(node: Node): boolean {
    while (isRightSideOfQualifiedNameOrPropertyAccess(node)) {
        node = node.parent;
    }
    return isCallExpression(node.parent) && node.parent.expression === node;
}

function isRightSideOfQualifiedNameOrPropertyAccess(node: Node): boolean {
    return (isQualifiedName(node.parent) && node.parent.right === node) || (isPropertyAccessExpression(node.parent) && node.parent.name === node);
}

const tokenFromDeclarationMapping = new Map<SyntaxKind, TokenType>([
    [SyntaxKind.VariableDeclaration, TokenType.variable],
    [SyntaxKind.Parameter, TokenType.parameter],
    [SyntaxKind.PropertyDeclaration, TokenType.property],
    [SyntaxKind.ModuleDeclaration, TokenType.namespace],
    [SyntaxKind.EnumDeclaration, TokenType.enum],
    [SyntaxKind.EnumMember, TokenType.enumMember],
    [SyntaxKind.ClassDeclaration, TokenType.class],
    [SyntaxKind.MethodDeclaration, TokenType.member],
    [SyntaxKind.FunctionDeclaration, TokenType.function],
    [SyntaxKind.FunctionExpression, TokenType.function],
    [SyntaxKind.MethodSignature, TokenType.member],
    [SyntaxKind.GetAccessor, TokenType.property],
    [SyntaxKind.SetAccessor, TokenType.property],
    [SyntaxKind.PropertySignature, TokenType.property],
    [SyntaxKind.InterfaceDeclaration, TokenType.interface],
    [SyntaxKind.TypeAliasDeclaration, TokenType.type],
    [SyntaxKind.TypeParameter, TokenType.typeParameter],
    [SyntaxKind.PropertyAssignment, TokenType.property],
    [SyntaxKind.ShorthandPropertyAssignment, TokenType.property]
]);
