import {
    __String,
    addToSeen,
    append,
    AssignmentDeclarationKind,
    BinaryExpression,
    BindingElement,
    Block,
    CallExpression,
    CancellationToken,
    canHaveSymbol,
    cast,
    CheckFlags,
    ClassLikeDeclaration,
    climbPastPropertyAccess,
    compareValues,
    ConstructorDeclaration,
    contains,
    createQueue,
    createTextSpan,
    createTextSpanFromBounds,
    createTextSpanFromRange,
    Debug,
    Declaration,
    displayPart,
    DocumentSpan,
    emptyArray,
    emptyOptions,
    escapeLeadingUnderscores,
    ExportSpecifier,
    Expression,
    externalHelpersModuleNameText,
    FileIncludeReason,
    FileReference,
    filter,
    find,
    findAncestor,
    findChildOfKind,
    findIndex,
    first,
    firstDefined,
    firstOrUndefined,
    flatMap,
    forEachChild,
    forEachChildRecursively,
    forEachReturnStatement,
    ForInOrOfStatement,
    FunctionDeclaration,
    FunctionExpression,
    FunctionLikeDeclaration,
    GetAccessorDeclaration,
    getAdjustedReferenceLocation,
    getAdjustedRenameLocation,
    getAllSuperTypeNodes,
    getAncestor,
    getAssignmentDeclarationKind,
    getCheckFlags,
    getContainerNode,
    getContainingObjectLiteralElement,
    getContextualTypeFromParentOrAncestorTypeNode,
    getDeclarationFromName,
    getDeclarationOfKind,
    getEffectiveModifierFlags,
    getLocalSymbolForExportDefault,
    getMeaningFromDeclaration,
    getMeaningFromLocation,
    getModeForUsageLocation,
    getNameOfDeclaration,
    getNameTable,
    getNextJSDocCommentLocation,
    getNodeId,
    getNodeKind,
    getPropertySymbolFromBindingElement,
    getPropertySymbolsFromContextualType,
    getQuoteFromPreference,
    getReferencedFileLocation,
    getSuperContainer,
    getSymbolId,
    getSyntacticModifierFlags,
    getTargetLabel,
    getTextOfNode,
    getThisContainer,
    getTouchingPropertyName,
    GoToDefinition,
    hasEffectiveModifier,
    hasInitializer,
    hasSyntacticModifier,
    hasType,
    HighlightSpan,
    HighlightSpanKind,
    Identifier,
    ImplementationLocation,
    InterfaceDeclaration,
    InternalSymbolName,
    isAccessExpression,
    isArrayLiteralOrObjectLiteralDestructuringPattern,
    isAssertionExpression,
    isBinaryExpression,
    isBindableObjectDefinePropertyCall,
    isBindingElement,
    isBreakOrContinueStatement,
    isCallExpression,
    isCallExpressionTarget,
    isCatchClause,
    isClassLike,
    isClassStaticBlockDeclaration,
    isComputedPropertyName,
    isConstructorDeclaration,
    isDeclaration,
    isDeclarationName,
    isExportAssignment,
    isExportSpecifier,
    isExpressionOfExternalModuleImportEqualsDeclaration,
    isExpressionStatement,
    isExpressionWithTypeArguments,
    isExternalModule,
    isExternalModuleSymbol,
    isExternalOrCommonJsModule,
    isForInOrOfStatement,
    isFunctionExpression,
    isFunctionLike,
    isFunctionLikeDeclaration,
    isIdentifier,
    isIdentifierPart,
    isImportMeta,
    isImportOrExportSpecifier,
    isImportSpecifier,
    isImportTypeNode,
    isInJSFile,
    isInNonReferenceComment,
    isInString,
    isInterfaceDeclaration,
    isJSDocMemberName,
    isJSDocTag,
    isJsxClosingElement,
    isJsxElement,
    isJsxFragment,
    isJsxOpeningElement,
    isJsxSelfClosingElement,
    isJumpStatementTarget,
    isLabeledStatement,
    isLabelOfLabeledStatement,
    isLiteralComputedPropertyDeclarationName,
    isLiteralNameOfPropertyDeclarationOrIndexAccess,
    isLiteralTypeNode,
    isMethodOrAccessor,
    isModuleDeclaration,
    isModuleExportsAccessExpression,
    isModuleOrEnumDeclaration,
    isModuleSpecifierLike,
    isNameOfModuleDeclaration,
    isNamespaceExportDeclaration,
    isNewExpressionTarget,
    isNoSubstitutionTemplateLiteral,
    isNumericLiteral,
    isObjectBindingElementWithoutPropertyName,
    isObjectLiteralExpression,
    isObjectLiteralMethod,
    isParameter,
    isParameterPropertyDeclaration,
    isPrivateIdentifierClassElementDeclaration,
    isPropertyAccessExpression,
    isQualifiedName,
    isReferencedFile,
    isReferenceFileLocation,
    isRightSideOfPropertyAccess,
    isShorthandPropertyAssignment,
    isSourceFile,
    isStatement,
    isStatic,
    isStaticModifier,
    isStringLiteralLike,
    isSuperProperty,
    isThis,
    isTypeAliasDeclaration,
    isTypeElement,
    isTypeKeyword,
    isTypeLiteralNode,
    isTypeNode,
    isTypeOperatorNode,
    isUnionTypeNode,
    isVariableDeclarationInitializedToBareOrAccessedRequire,
    isVariableDeclarationList,
    isVariableLike,
    isVariableStatement,
    isVoidExpression,
    isWriteAccess,
    JSDocTag,
    map,
    mapDefined,
    MethodDeclaration,
    ModifierFlags,
    ModuleDeclaration,
    MultiMap,
    NamedDeclaration,
    Node,
    NodeFlags,
    nodeSeenTracker,
    NumericLiteral,
    ObjectLiteralExpression,
    or,
    ParameterDeclaration,
    ParenthesizedExpression,
    Path,
    PrivateIdentifier,
    Program,
    PropertyAccessExpression,
    PropertyAssignment,
    PropertyDeclaration,
    punctuationPart,
    QuotePreference,
    rangeIsOnSingleLine,
    ReferencedSymbol,
    ReferencedSymbolDefinitionInfo,
    ReferencedSymbolEntry,
    ReferenceEntry,
    RenameLocation,
    ScriptElementKind,
    ScriptTarget,
    SemanticMeaning,
    SetAccessorDeclaration,
    SignatureDeclaration,
    skipAlias,
    some,
    SourceFile,
    Statement,
    StringLiteral,
    StringLiteralLike,
    stripQuotes,
    SuperContainer,
    Symbol,
    SymbolDisplay,
    SymbolDisplayPart,
    SymbolDisplayPartKind,
    SymbolFlags,
    SymbolId,
    symbolName,
    SyntaxKind,
    textPart,
    TextSpan,
    tokenToString,
    TransformFlags,
    tryAddToSet,
    tryCast,
    tryGetClassExtendingExpressionWithTypeArguments,
    tryGetImportFromModuleSpecifier,
    TypeChecker,
    TypeLiteralNode,
    VariableDeclaration,
} from "./_namespaces/ts";
import {
    createImportTracker,
    ExportInfo,
    ExportKind,
    findModuleReferences,
    getExportInfo,
    getImportOrExportSymbol,
    ImportExport,
    ImportsResult,
    ImportTracker,
    ModuleReference,
} from "./_namespaces/ts.FindAllReferences";

/** @internal */
export interface SymbolAndEntries {
    readonly definition: Definition | undefined;
    readonly references: readonly Entry[];
}

/** @internal */
export const enum DefinitionKind { Symbol, Label, Keyword, This, String, TripleSlashReference }
/** @internal */
export type Definition =
    | { readonly type: DefinitionKind.Symbol; readonly symbol: Symbol }
    | { readonly type: DefinitionKind.Label; readonly node: Identifier }
    | { readonly type: DefinitionKind.Keyword; readonly node: Node }
    | { readonly type: DefinitionKind.This; readonly node: Node }
    | { readonly type: DefinitionKind.String; readonly node: StringLiteralLike }
    | { readonly type: DefinitionKind.TripleSlashReference; readonly reference: FileReference, readonly file: SourceFile };

/** @internal */
export const enum EntryKind { Span, Node, StringLiteral, SearchedLocalFoundProperty, SearchedPropertyFoundLocal }
/** @internal */
export type NodeEntryKind = EntryKind.Node | EntryKind.StringLiteral | EntryKind.SearchedLocalFoundProperty | EntryKind.SearchedPropertyFoundLocal;
/** @internal */
export type Entry = NodeEntry | SpanEntry;
/** @internal */
export interface ContextWithStartAndEndNode {
    start: Node;
    end: Node;
}
/** @internal */
export type ContextNode = Node | ContextWithStartAndEndNode;
/** @internal */
export interface NodeEntry {
    readonly kind: NodeEntryKind;
    readonly node: Node;
    readonly context?: ContextNode;
}
/** @internal */
export interface SpanEntry {
    readonly kind: EntryKind.Span;
    readonly fileName: string;
    readonly textSpan: TextSpan;
}
/** @internal */
export function nodeEntry(node: Node, kind: NodeEntryKind = EntryKind.Node): NodeEntry {
    return {
        kind,
        node: (node as NamedDeclaration).name || node,
        context: getContextNodeForNodeEntry(node)
    };
}

/** @internal */
export function isContextWithStartAndEndNode(node: ContextNode): node is ContextWithStartAndEndNode {
    return node && (node as Node).kind === undefined;
}

/**
 * Returns the context node for a given node entry.
 * @param {Node} node - The node to get the context node for.
 * @returns {ContextNode | undefined} - The context node or undefined if not found.
 * @remarks This function handles various scenarios such as declarations, JSX tags, computed property names, and more.
 */
function getContextNodeForNodeEntry(node: Node): ContextNode | undefined {
    if (isDeclaration(node)) {
        return getContextNode(node);
    }

    if (!node.parent) return undefined;

    if (!isDeclaration(node.parent) && !isExportAssignment(node.parent)) {
        // Special property assignment in javascript
        if (isInJSFile(node)) {
            const binaryExpression = isBinaryExpression(node.parent) ?
                node.parent :
                isAccessExpression(node.parent) &&
                    isBinaryExpression(node.parent.parent) &&
                    node.parent.parent.left === node.parent ?
                    node.parent.parent :
                    undefined;
            if (binaryExpression && getAssignmentDeclarationKind(binaryExpression) !== AssignmentDeclarationKind.None) {
                return getContextNode(binaryExpression);
            }
        }

        // Jsx Tags
        if (isJsxOpeningElement(node.parent) || isJsxClosingElement(node.parent)) {
            return node.parent.parent;
        }
        else if (isJsxSelfClosingElement(node.parent) ||
            isLabeledStatement(node.parent) ||
            isBreakOrContinueStatement(node.parent)) {
            return node.parent;
        }
        else if (isStringLiteralLike(node)) {
            const validImport = tryGetImportFromModuleSpecifier(node);
            if (validImport) {
                const declOrStatement = findAncestor(validImport, node =>
                    isDeclaration(node) ||
                    isStatement(node) ||
                    isJSDocTag(node)
                )! as NamedDeclaration | Statement | JSDocTag;
                return isDeclaration(declOrStatement) ?
                    getContextNode(declOrStatement) :
                    declOrStatement;
            }
        }

        // Handle computed property name
        const propertyName = findAncestor(node, isComputedPropertyName);
        return propertyName ?
            getContextNode(propertyName.parent) :
            undefined;
    }

    if (node.parent.name === node || // node is name of declaration, use parent
        isConstructorDeclaration(node.parent) ||
        isExportAssignment(node.parent) ||
        // Property name of the import export specifier or binding pattern, use parent
        ((isImportOrExportSpecifier(node.parent) || isBindingElement(node.parent))
            && node.parent.propertyName === node) ||
        // Is default export
        (node.kind === SyntaxKind.DefaultKeyword && hasSyntacticModifier(node.parent, ModifierFlags.ExportDefault))) {
        return getContextNode(node.parent);
    }

    return undefined;
}

/**
 * Returns the context node for a given NamedDeclaration, BinaryExpression, or ForInOrOfStatement node.
 * @internal
 * @param {NamedDeclaration | BinaryExpression | ForInOrOfStatement | undefined} node - The node to get the context node for.
 * @returns {ContextNode | undefined} The context node for the given node, or undefined if the node is falsy.
 */
export function getContextNode(node: NamedDeclaration | BinaryExpression | ForInOrOfStatement | undefined): ContextNode | undefined {
    if (!node) return undefined;
    switch (node.kind) {
        case SyntaxKind.VariableDeclaration:
            return !isVariableDeclarationList(node.parent) || node.parent.declarations.length !== 1 ?
                node :
                isVariableStatement(node.parent.parent) ?
                    node.parent.parent :
                    isForInOrOfStatement(node.parent.parent) ?
                        getContextNode(node.parent.parent) :
                        node.parent;

        case SyntaxKind.BindingElement:
            return getContextNode(node.parent.parent as NamedDeclaration);

        case SyntaxKind.ImportSpecifier:
            return node.parent.parent.parent;

        case SyntaxKind.ExportSpecifier:
        case SyntaxKind.NamespaceImport:
            return node.parent.parent;

        case SyntaxKind.ImportClause:
        case SyntaxKind.NamespaceExport:
            return node.parent;

        case SyntaxKind.BinaryExpression:
            return isExpressionStatement(node.parent) ?
                node.parent :
                node;

        case SyntaxKind.ForOfStatement:
        case SyntaxKind.ForInStatement:
            return {
                start: (node as ForInOrOfStatement).initializer,
                end: (node as ForInOrOfStatement).expression
            };

        case SyntaxKind.PropertyAssignment:
        case SyntaxKind.ShorthandPropertyAssignment:
            return isArrayLiteralOrObjectLiteralDestructuringPattern(node.parent) ?
                getContextNode(
                    findAncestor(node.parent, node =>
                        isBinaryExpression(node) || isForInOrOfStatement(node)
                    ) as BinaryExpression | ForInOrOfStatement
                ) :
                node;

        default:
            return node;
    }
}

/** @internal */
export function toContextSpan(textSpan: TextSpan, sourceFile: SourceFile, context?: ContextNode): { contextSpan: TextSpan } | undefined {
    if (!context) return undefined;
    const contextSpan = isContextWithStartAndEndNode(context) ?
        getTextSpan(context.start, sourceFile, context.end) :
        getTextSpan(context, sourceFile);
    return contextSpan.start !== textSpan.start || contextSpan.length !== textSpan.length ?
        { contextSpan } :
        undefined;
}

/**
 * An enumeration representing the different ways to search for references to a symbol.
 * @enum {number}
 * @property {number} Other - When searching for references to a symbol, the location will not be adjusted (this is the default behavior when not specified).
 * @property {number} References - When searching for references to a symbol, the location will be adjusted if the cursor was on a keyword.
 * @property {number} Rename - When searching for references to a symbol, the location will be adjusted if the cursor was on a keyword.
 * Unlike `References`, the location will only be adjusted keyword belonged to a declaration with a valid name.
 * If set, we will find fewer references -- if it is referenced by several different names, we still only find references for the original name.
 */
export const enum FindReferencesUse {
    /**
     * When searching for references to a symbol, the location will not be adjusted (this is the default behavior when not specified).
     */
    Other,
    /**
     * When searching for references to a symbol, the location will be adjusted if the cursor was on a keyword.
     */
    References,
    /**
     * When searching for references to a symbol, the location will be adjusted if the cursor was on a keyword.
     * Unlike `References`, the location will only be adjusted keyword belonged to a declaration with a valid name.
     * If set, we will find fewer references -- if it is referenced by several different names, we still only find references for the original name.
     */
    Rename,
}

/**
 * Options for finding references in TypeScript code.
 * @param {boolean} [findInStrings] - Whether to search for references in string literals. Defaults to false.
 * @param {boolean} [findInComments] - Whether to search for references in comments. Defaults to false.
 * @param {FindReferencesUse} [use] - The type of references to search for. Defaults to 'references'.
 * @param {boolean} [implementations] - Whether to search for implementations instead of references. Defaults to false.
 * @param {boolean} [providePrefixAndSuffixTextForRename] - Whether to opt in for enhanced renaming of shorthand properties and import/export specifiers. Defaults to false.
 */
export interface Options {
    readonly findInStrings?: boolean;
    readonly findInComments?: boolean;
    readonly use?: FindReferencesUse;
    /** True if we are searching for implementations. We will have a different method of adding references if so. */
    readonly implementations?: boolean;
    /**
     * True to opt in for enhanced renaming of shorthand properties and import/export specifiers.
     * The options controls the behavior for the whole rename operation; it cannot be changed on a per-file basis.
     * Default is false for backwards compatibility.
     */
    readonly providePrefixAndSuffixTextForRename?: boolean;
}

/**
 * Finds referenced symbols for a given position in a source file.
 * @param program - The TypeScript program.
 * @param cancellationToken - The cancellation token.
 * @param sourceFiles - The source files to search.
 * @param sourceFile - The source file to search in.
 * @param position - The position to search at.
 * @returns An array of referenced symbols or undefined if none are found.
 */
export function findReferencedSymbols(program: Program, cancellationToken: CancellationToken, sourceFiles: readonly SourceFile[], sourceFile: SourceFile, position: number): ReferencedSymbol[] | undefined {
    const node = getTouchingPropertyName(sourceFile, position);
    const options = { use: FindReferencesUse.References };
    const referencedSymbols = Core.getReferencedSymbolsForNode(position, node, program, sourceFiles, cancellationToken, options);
    const checker = program.getTypeChecker();
    // Unless the starting node is a declaration (vs e.g. JSDoc), don't attempt to compute isDefinition
    const adjustedNode = Core.getAdjustedNode(node, options);
    const symbol = isDefinitionForReference(adjustedNode) ? checker.getSymbolAtLocation(adjustedNode) : undefined;
    return !referencedSymbols || !referencedSymbols.length ? undefined : mapDefined<SymbolAndEntries, ReferencedSymbol>(referencedSymbols, ({ definition, references }) =>
        // Only include referenced symbols that have a valid definition.
        definition && {
            definition: checker.runWithCancellationToken(cancellationToken, checker => definitionToReferencedSymbolDefinitionInfo(definition, checker, node)),
            references: references.map(r => toReferencedSymbolEntry(r, symbol))
        });
}

function isDefinitionForReference(node: Node): boolean {
    return node.kind === SyntaxKind.DefaultKeyword
        || !!getDeclarationFromName(node)
        || isLiteralComputedPropertyDeclarationName(node)
        || (node.kind === SyntaxKind.ConstructorKeyword && isConstructorDeclaration(node.parent));
}

/**
 * Returns an array of ImplementationLocation objects for the given program, source files, source file, and position.
 * @param program - The Program object.
 * @param cancellationToken - The CancellationToken object.
 * @param sourceFiles - An array of SourceFile objects.
 * @param sourceFile - The SourceFile object.
 * @param position - The position in the source file.
 * @returns An array of ImplementationLocation objects or undefined.
 */
export function getImplementationsAtPosition(program: Program, cancellationToken: CancellationToken, sourceFiles: readonly SourceFile[], sourceFile: SourceFile, position: number): ImplementationLocation[] | undefined {
    const node = getTouchingPropertyName(sourceFile, position);
    let referenceEntries: Entry[] | undefined;
    const entries = getImplementationReferenceEntries(program, cancellationToken, sourceFiles, node, position);

    if (
        node.parent.kind === SyntaxKind.PropertyAccessExpression
        || node.parent.kind === SyntaxKind.BindingElement
        || node.parent.kind === SyntaxKind.ElementAccessExpression
        || node.kind === SyntaxKind.SuperKeyword
    ) {
        referenceEntries = entries && [...entries];
    }
    else if (entries) {
        const queue = createQueue(entries);
        const seenNodes = new Map<number, true>();
        while (!queue.isEmpty()) {
            const entry = queue.dequeue() as NodeEntry;
            if (!addToSeen(seenNodes, getNodeId(entry.node))) {
                continue;
            }
            referenceEntries = append(referenceEntries, entry);
            const entries = getImplementationReferenceEntries(program, cancellationToken, sourceFiles, entry.node, entry.node.pos);
            if (entries) {
                queue.enqueue(...entries);
            }
        }
    }
    const checker = program.getTypeChecker();
    return map(referenceEntries, entry => toImplementationLocation(entry, checker));
}

/**
 * Retrieves the implementation reference entries for a given node in a program.
 * @param program - The program to search for references in.
 * @param cancellationToken - A token that can be used to cancel the operation.
 * @param sourceFiles - The source files to search for references in.
 * @param node - The node to retrieve implementation reference entries for.
 * @param position - The position of the node in the source file.
 * @returns An array of Entry objects representing the implementation reference entries for the given node, or undefined if the node is a SourceFile or no implementation reference entries were found.
 */
function getImplementationReferenceEntries(program: Program, cancellationToken: CancellationToken, sourceFiles: readonly SourceFile[], node: Node, position: number): readonly Entry[] | undefined {
    if (node.kind === SyntaxKind.SourceFile) {
        return undefined;
    }

    const checker = program.getTypeChecker();
    // If invoked directly on a shorthand property assignment, then return
    // the declaration of the symbol being assigned (not the symbol being assigned to).
    if (node.parent.kind === SyntaxKind.ShorthandPropertyAssignment) {
        const result: NodeEntry[] = [];
        Core.getReferenceEntriesForShorthandPropertyAssignment(node, checker, node => result.push(nodeEntry(node)));
        return result;
    }
    else if (node.kind === SyntaxKind.SuperKeyword || isSuperProperty(node.parent)) {
        // References to and accesses on the super keyword only have one possible implementation, so no
        // need to "Find all References"
        const symbol = checker.getSymbolAtLocation(node)!;
        return symbol.valueDeclaration && [nodeEntry(symbol.valueDeclaration)];
    }
    else {
        // Perform "Find all References" and retrieve only those that are implementations
        return getReferenceEntriesForNode(position, node, program, sourceFiles, cancellationToken, { implementations: true, use: FindReferencesUse.References });
    }
}

/** @internal */
export function findReferenceOrRenameEntries<T>(
    program: Program, cancellationToken: CancellationToken, sourceFiles: readonly SourceFile[], node: Node, position: number, options: Options | undefined,
    convertEntry: ToReferenceOrRenameEntry<T>,
): T[] | undefined {
    return map(flattenEntries(Core.getReferencedSymbolsForNode(position, node, program, sourceFiles, cancellationToken, options)), entry => convertEntry(entry, node, program.getTypeChecker()));
}

/** @internal */
export type ToReferenceOrRenameEntry<T> = (entry: Entry, originalNode: Node, checker: TypeChecker) => T;

/**
 * Retrieves reference entries for a given node.
 * @param position - The position of the node.
 * @param node - The node to retrieve reference entries for.
 * @param program - The program containing the node.
 * @param sourceFiles - An array of source files to search for references in.
 * @param cancellationToken - A token used to cancel the operation.
 * @param options - Optional options to customize the search.
 * @param sourceFilesSet - Optional set of source file names to limit the search to.
 * @returns An array of reference entries or undefined if none were found.
 */
export function getReferenceEntriesForNode(
    position: number,
    node: Node,
    program: Program,
    sourceFiles: readonly SourceFile[],
    cancellationToken: CancellationToken,
    options: Options = {},
    sourceFilesSet: ReadonlySet<string> = new Set(sourceFiles.map(f => f.fileName)),
): readonly Entry[] | undefined {
    return flattenEntries(Core.getReferencedSymbolsForNode(position, node, program, sourceFiles, cancellationToken, options, sourceFilesSet));
}

function flattenEntries(referenceSymbols: readonly SymbolAndEntries[] | undefined): readonly Entry[] | undefined {
    return referenceSymbols && flatMap(referenceSymbols, r => r.references);
}

/**
 * Returns a ReferencedSymbolDefinitionInfo object based on the provided Definition, TypeChecker, and Node.
 * @param {Definition} def - The Definition object to use.
 * @param {TypeChecker} checker - The TypeChecker object to use.
 * @param {Node} originalNode - The original Node object to use.
 * @returns {ReferencedSymbolDefinitionInfo} - The resulting ReferencedSymbolDefinitionInfo object.
 */
function definitionToReferencedSymbolDefinitionInfo(def: Definition, checker: TypeChecker, originalNode: Node): ReferencedSymbolDefinitionInfo {
    const info = ((): { sourceFile: SourceFile, textSpan: TextSpan, name: string, kind: ScriptElementKind, displayParts: SymbolDisplayPart[], context?: Node | ContextWithStartAndEndNode } => {
        switch (def.type) {
            case DefinitionKind.Symbol: {
                const { symbol } = def;
                const { displayParts, kind } = getDefinitionKindAndDisplayParts(symbol, checker, originalNode);
                const name = displayParts.map(p => p.text).join("");
                const declaration = symbol.declarations && firstOrUndefined(symbol.declarations);
                const node = declaration ? (getNameOfDeclaration(declaration) || declaration) : originalNode;
                return {
                    ...getFileAndTextSpanFromNode(node),
                    name,
                    kind,
                    displayParts,
                    context: getContextNode(declaration)
                };
            }
            case DefinitionKind.Label: {
                const { node } = def;
                return { ...getFileAndTextSpanFromNode(node), name: node.text, kind: ScriptElementKind.label, displayParts: [displayPart(node.text, SymbolDisplayPartKind.text)] };
            }
            case DefinitionKind.Keyword: {
                const { node } = def;
                const name = tokenToString(node.kind)!;
                return { ...getFileAndTextSpanFromNode(node), name, kind: ScriptElementKind.keyword, displayParts: [{ text: name, kind: ScriptElementKind.keyword }] };
            }
            case DefinitionKind.This: {
                const { node } = def;
                const symbol = checker.getSymbolAtLocation(node);
                const displayParts = symbol && SymbolDisplay.getSymbolDisplayPartsDocumentationAndSymbolKind(
                    checker, symbol, node.getSourceFile(), getContainerNode(node), node).displayParts || [textPart("this")];
                return { ...getFileAndTextSpanFromNode(node), name: "this", kind: ScriptElementKind.variableElement, displayParts };
            }
            case DefinitionKind.String: {
                const { node } = def;
                return {
                    ...getFileAndTextSpanFromNode(node),
                    name: node.text,
                    kind: ScriptElementKind.variableElement,
                    displayParts: [displayPart(getTextOfNode(node), SymbolDisplayPartKind.stringLiteral)]
                };
            }
            case DefinitionKind.TripleSlashReference: {
                return {
                    textSpan: createTextSpanFromRange(def.reference),
                    sourceFile: def.file,
                    name: def.reference.fileName,
                    kind: ScriptElementKind.string,
                    displayParts: [displayPart(`"${def.reference.fileName}"`, SymbolDisplayPartKind.stringLiteral)]
                };
            }
            default:
                return Debug.assertNever(def);
        }
    })();

    const { sourceFile, textSpan, name, kind, displayParts, context } = info;
    return {
        containerKind: ScriptElementKind.unknown,
        containerName: "",
        fileName: sourceFile.fileName,
        kind,
        name,
        textSpan,
        displayParts,
        ...toContextSpan(textSpan, sourceFile, context)
    };
}

function getFileAndTextSpanFromNode(node: Node) {
    const sourceFile = node.getSourceFile();
    return {
        sourceFile,
        textSpan: getTextSpan(isComputedPropertyName(node) ? node.expression : node, sourceFile)
    };
}

function getDefinitionKindAndDisplayParts(symbol: Symbol, checker: TypeChecker, node: Node): { displayParts: SymbolDisplayPart[], kind: ScriptElementKind } {
    const meaning = Core.getIntersectingMeaningFromDeclarations(node, symbol);
    const enclosingDeclaration = symbol.declarations && firstOrUndefined(symbol.declarations) || node;
    const { displayParts, symbolKind } =
        SymbolDisplay.getSymbolDisplayPartsDocumentationAndSymbolKind(checker, symbol, enclosingDeclaration.getSourceFile(), enclosingDeclaration, enclosingDeclaration, meaning);
    return { displayParts, kind: symbolKind };
}

/** @internal */
export function toRenameLocation(entry: Entry, originalNode: Node, checker: TypeChecker, providePrefixAndSuffixText: boolean, quotePreference: QuotePreference): RenameLocation {
    return { ...entryToDocumentSpan(entry), ...(providePrefixAndSuffixText && getPrefixAndSuffixText(entry, originalNode, checker, quotePreference)) };
}

function toReferencedSymbolEntry(entry: Entry, symbol: Symbol | undefined): ReferencedSymbolEntry {
    const referenceEntry = toReferenceEntry(entry);
    if (!symbol) return referenceEntry;
    return {
        ...referenceEntry,
        isDefinition: entry.kind !== EntryKind.Span && isDeclarationOfSymbol(entry.node, symbol)
    };
}

/**
 * Converts an Entry object to a ReferenceEntry object.
 * @param {Entry} entry - The Entry object to convert.
 * @returns {ReferenceEntry} The converted ReferenceEntry object.
 * @remarks This function is marked as internal.
 */
export function toReferenceEntry(entry: Entry): ReferenceEntry {
    const documentSpan = entryToDocumentSpan(entry);
    if (entry.kind === EntryKind.Span) {
        return { ...documentSpan, isWriteAccess: false };
    }
    const { kind, node } = entry;
    return {
        ...documentSpan,
        isWriteAccess: isWriteAccessForReference(node),
        isInString: kind === EntryKind.StringLiteral ? true : undefined,
    };
}

/**
 * Converts an Entry object to a DocumentSpan object.
 * @param {Entry} entry - The Entry object to be converted.
 * @returns {DocumentSpan} - The resulting DocumentSpan object.
 */
function entryToDocumentSpan(entry: Entry): DocumentSpan {
    if (entry.kind === EntryKind.Span) {
        return { textSpan: entry.textSpan, fileName: entry.fileName };
    }
    else {
        const sourceFile = entry.node.getSourceFile();
        const textSpan = getTextSpan(entry.node, sourceFile);
        return {
            textSpan,
            fileName: sourceFile.fileName,
            ...toContextSpan(textSpan, sourceFile, entry.context)
        };
    }
}

interface PrefixAndSuffix { readonly prefixText?: string; readonly suffixText?: string; }
/**
 * Returns an object containing prefix and suffix text for a given entry, original node, checker, and quote preference.
 * @param {Entry} entry - The entry to get prefix and suffix text for.
 * @param {Node} originalNode - The original node.
 * @param {TypeChecker} checker - The type checker.
 * @param {QuotePreference} quotePreference - The quote preference.
 * @returns {PrefixAndSuffix} An object containing prefix and suffix text.
 */
function getPrefixAndSuffixText(entry: Entry, originalNode: Node, checker: TypeChecker, quotePreference: QuotePreference): PrefixAndSuffix {
    if (entry.kind !== EntryKind.Span && isIdentifier(originalNode)) {
        const { node, kind } = entry;
        const parent = node.parent;
        const name = originalNode.text;
        const isShorthandAssignment = isShorthandPropertyAssignment(parent);
        if (isShorthandAssignment || (isObjectBindingElementWithoutPropertyName(parent) && parent.name === node && parent.dotDotDotToken === undefined)) {
            const prefixColon: PrefixAndSuffix = { prefixText: name + ": " };
            const suffixColon: PrefixAndSuffix = { suffixText: ": " + name };
            if (kind === EntryKind.SearchedLocalFoundProperty) {
                return prefixColon;
            }
            if (kind === EntryKind.SearchedPropertyFoundLocal) {
                return suffixColon;
            }

            // In `const o = { x }; o.x`, symbolAtLocation at `x` in `{ x }` is the property symbol.
            // For a binding element `const { x } = o;`, symbolAtLocation at `x` is the property symbol.
            if (isShorthandAssignment) {
                const grandParent = parent.parent;
                if (isObjectLiteralExpression(grandParent) &&
                    isBinaryExpression(grandParent.parent) &&
                    isModuleExportsAccessExpression(grandParent.parent.left)) {
                    return prefixColon;
                }
                return suffixColon;
            }
            else {
                return prefixColon;
            }
        }
        else if (isImportSpecifier(parent) && !parent.propertyName) {
            // If the original symbol was using this alias, just rename the alias.
            const originalSymbol = isExportSpecifier(originalNode.parent) ? checker.getExportSpecifierLocalTargetSymbol(originalNode.parent) : checker.getSymbolAtLocation(originalNode);
            return contains(originalSymbol!.declarations, parent) ? { prefixText: name + " as " } : emptyOptions;
        }
        else if (isExportSpecifier(parent) && !parent.propertyName) {
            // If the symbol for the node is same as declared node symbol use prefix text
            return originalNode === entry.node || checker.getSymbolAtLocation(originalNode) === checker.getSymbolAtLocation(entry.node) ?
                { prefixText: name + " as " } :
                { suffixText: " as " + name };
        }
    }

    // If the node is a numerical indexing literal, then add quotes around the property access.
    if (entry.kind !== EntryKind.Span && isNumericLiteral(entry.node) && isAccessExpression(entry.node.parent)) {
        const quote = getQuoteFromPreference(quotePreference);
        return { prefixText: quote, suffixText: quote };
    }

    return emptyOptions;
}

/**
 * Returns an ImplementationLocation object based on the provided Entry and TypeChecker.
 * @param {Entry} entry - The Entry object to be converted to an ImplementationLocation.
 * @param {TypeChecker} checker - The TypeChecker object to be used in the conversion process.
 * @returns {ImplementationLocation} - The resulting ImplementationLocation object.
 */
function toImplementationLocation(entry: Entry, checker: TypeChecker): ImplementationLocation {
    const documentSpan = entryToDocumentSpan(entry);
    if (entry.kind !== EntryKind.Span) {
        const { node } = entry;
        return {
            ...documentSpan,
            ...implementationKindDisplayParts(node, checker)
        };
    }
    else {
        return { ...documentSpan, kind: ScriptElementKind.unknown, displayParts: [] };
    }
}

/**
 * Returns the kind and display parts of a given node's implementation. If the node is a symbol, it retrieves the definition kind and display parts using the provided TypeChecker. If the node is an object literal expression, it returns an interface element with display parts indicating it is an object literal. If the node is a class expression, it returns a local class element with display parts indicating it is an anonymous local class. Otherwise, it returns the kind of the node and an empty array of display parts.
 * @param node The node to retrieve the implementation kind and display parts of.
 * @param checker The TypeChecker to use for retrieving the definition kind and display parts of a symbol.
 * @returns An object containing the kind and display parts of the node's implementation.
 */
function implementationKindDisplayParts(node: Node, checker: TypeChecker): { kind: ScriptElementKind, displayParts: SymbolDisplayPart[] } {
    const symbol = checker.getSymbolAtLocation(isDeclaration(node) && node.name ? node.name : node);
    if (symbol) {
        return getDefinitionKindAndDisplayParts(symbol, checker, node);
    }
    else if (node.kind === SyntaxKind.ObjectLiteralExpression) {
        return {
            kind: ScriptElementKind.interfaceElement,
            displayParts: [punctuationPart(SyntaxKind.OpenParenToken), textPart("object literal"), punctuationPart(SyntaxKind.CloseParenToken)]
        };
    }
    else if (node.kind === SyntaxKind.ClassExpression) {
        return {
            kind: ScriptElementKind.localClassElement,
            displayParts: [punctuationPart(SyntaxKind.OpenParenToken), textPart("anonymous local class"), punctuationPart(SyntaxKind.CloseParenToken)]
        };
    }
    else {
        return { kind: getNodeKind(node), displayParts: [] };
    }
}

/**
 * Converts an Entry object to a HighlightSpan object with fileName and span properties.
 * @param entry - The Entry object to convert.
 * @returns An object with fileName and span properties.
 */
export function toHighlightSpan(entry: Entry): { fileName: string, span: HighlightSpan } {
    const documentSpan = entryToDocumentSpan(entry);
    if (entry.kind === EntryKind.Span) {
        return {
            fileName: documentSpan.fileName,
            span: {
                textSpan: documentSpan.textSpan,
                kind: HighlightSpanKind.reference
            }
        };
    }

    const writeAccess = isWriteAccessForReference(entry.node);
    const span: HighlightSpan = {
        textSpan: documentSpan.textSpan,
        kind: writeAccess ? HighlightSpanKind.writtenReference : HighlightSpanKind.reference,
        isInString: entry.kind === EntryKind.StringLiteral ? true : undefined,
        ...documentSpan.contextSpan && { contextSpan: documentSpan.contextSpan }
    };
    return { fileName: documentSpan.fileName, span };
}

/**
 * Returns a TextSpan object based on the provided Node and SourceFile.
 * @param {Node} node - The Node to get the start position from.
 * @param {SourceFile} sourceFile - The SourceFile to get the start position from.
 * @param {Node} [endNode] - Optional end Node to get the end position from.
 * @returns {TextSpan} - The TextSpan object containing the start and end positions.
 */
function getTextSpan(node: Node, sourceFile: SourceFile, endNode?: Node): TextSpan {
    let start = node.getStart(sourceFile);
    let end = (endNode || node).getEnd();
    if (isStringLiteralLike(node) && (end - start) > 2) {
        Debug.assert(endNode === undefined);
        start += 1;
        end -= 1;
    }
    return createTextSpanFromBounds(start, end);
}

/** @internal */
export function getTextSpanOfEntry(entry: Entry) {
    return entry.kind === EntryKind.Span ? entry.textSpan :
        getTextSpan(entry.node, entry.node.getSourceFile());
}

/**
 * A node is considered a writeAccess iff it is a name of a declaration or a target of an assignment.
 *
 * @internal
 */
export function isWriteAccessForReference(node: Node): boolean {
    const decl = getDeclarationFromName(node);
    return !!decl && declarationIsWriteAccess(decl) || node.kind === SyntaxKind.DefaultKeyword || isWriteAccess(node);
}

/**
 * Determines whether a given reference node is a definition of the target symbol.
 * @param node - The reference node to check.
 * @param target - The symbol to check against.
 * @returns A boolean indicating whether the reference node is a definition of the target symbol.
 * @internal
 */
export function isDeclarationOfSymbol(node: Node, target: Symbol | undefined): boolean {
    if (!target) return false;
    const source = getDeclarationFromName(node) ||
        (node.kind === SyntaxKind.DefaultKeyword ? node.parent
        : isLiteralComputedPropertyDeclarationName(node) ? node.parent.parent
        : node.kind === SyntaxKind.ConstructorKeyword && isConstructorDeclaration(node.parent) ? node.parent.parent
        : undefined);
    const commonjsSource = source && isBinaryExpression(source) ? source.left as unknown as Declaration : undefined;
    return !!(source && target.declarations?.some(d => d === source || d === commonjsSource));
}

/**
 * Determines if a given declaration provides a value or is just a location for a future write.
 * @param decl - The declaration to check.
 * @returns True if the declaration provides a value, false otherwise.
 */
function declarationIsWriteAccess(decl: Declaration): boolean {
    // Consider anything in an ambient declaration to be a write access since it may be coming from JS.
    if (!!(decl.flags & NodeFlags.Ambient)) return true;

    switch (decl.kind) {
        case SyntaxKind.BinaryExpression:
        case SyntaxKind.BindingElement:
        case SyntaxKind.ClassDeclaration:
        case SyntaxKind.ClassExpression:
        case SyntaxKind.DefaultKeyword:
        case SyntaxKind.EnumDeclaration:
        case SyntaxKind.EnumMember:
        case SyntaxKind.ExportSpecifier:
        case SyntaxKind.ImportClause: // default import
        case SyntaxKind.ImportEqualsDeclaration:
        case SyntaxKind.ImportSpecifier:
        case SyntaxKind.InterfaceDeclaration:
        case SyntaxKind.JSDocCallbackTag:
        case SyntaxKind.JSDocTypedefTag:
        case SyntaxKind.JsxAttribute:
        case SyntaxKind.ModuleDeclaration:
        case SyntaxKind.NamespaceExportDeclaration:
        case SyntaxKind.NamespaceImport:
        case SyntaxKind.NamespaceExport:
        case SyntaxKind.Parameter:
        case SyntaxKind.ShorthandPropertyAssignment:
        case SyntaxKind.TypeAliasDeclaration:
        case SyntaxKind.TypeParameter:
            return true;

        case SyntaxKind.PropertyAssignment:
            // In `({ x: y } = 0);`, `x` is not a write access. (Won't call this function for `y`.)
            return !isArrayLiteralOrObjectLiteralDestructuringPattern((decl as PropertyAssignment).parent);

        case SyntaxKind.FunctionDeclaration:
        case SyntaxKind.FunctionExpression:
        case SyntaxKind.Constructor:
        case SyntaxKind.MethodDeclaration:
        case SyntaxKind.GetAccessor:
        case SyntaxKind.SetAccessor:
            return !!(decl as FunctionDeclaration | FunctionExpression | ConstructorDeclaration | MethodDeclaration | GetAccessorDeclaration | SetAccessorDeclaration).body;

        case SyntaxKind.VariableDeclaration:
        case SyntaxKind.PropertyDeclaration:
            return !!(decl as VariableDeclaration | PropertyDeclaration).initializer || isCatchClause(decl.parent);

        case SyntaxKind.MethodSignature:
        case SyntaxKind.PropertySignature:
        case SyntaxKind.JSDocPropertyTag:
        case SyntaxKind.JSDocParameterTag:
            return false;

        default:
            return Debug.failBadSyntaxKind(decl);
    }
}

/**
 * Encapsulates the core find-all-references algorithm.
 *
 * @internal
 */
export namespace Core {
    /**
     * Retrieves all symbols referenced by a given node in a TypeScript program.
     * @param position The position of the node in the source file.
     * @param node The node to retrieve referenced symbols for.
     * @param program The TypeScript program.
     * @param sourceFiles An array of source files to search for references in.
     * @param cancellationToken A token used to cancel the operation.
     * @param options Optional options for the operation.
     * @param sourceFilesSet A set of source file names to limit the search to.
     * @returns An array of SymbolAndEntries objects representing the referenced symbols and their references.
     */
    export function getReferencedSymbolsForNode(position: number, node: Node, program: Program, sourceFiles: readonly SourceFile[], cancellationToken: CancellationToken, options: Options = {}, sourceFilesSet: ReadonlySet<string> = new Set(sourceFiles.map(f => f.fileName))): readonly SymbolAndEntries[] | undefined {
        node = getAdjustedNode(node, options);
        if (isSourceFile(node)) {
            const resolvedRef = GoToDefinition.getReferenceAtPosition(node, position, program);
            if (!resolvedRef?.file) {
                return undefined;
            }
            const moduleSymbol = program.getTypeChecker().getMergedSymbol(resolvedRef.file.symbol);
            if (moduleSymbol) {
                return getReferencedSymbolsForModule(program, moduleSymbol, /*excludeImportTypeOfExportEquals*/ false, sourceFiles, sourceFilesSet);
            }
            const fileIncludeReasons = program.getFileIncludeReasons();
            if (!fileIncludeReasons) {
                return undefined;
            }
            return [{
                definition: { type: DefinitionKind.TripleSlashReference, reference: resolvedRef.reference, file: node },
                references: getReferencesForNonModule(resolvedRef.file, fileIncludeReasons, program) || emptyArray
            }];
        }

        if (!options.implementations) {
            const special = getReferencedSymbolsSpecial(node, sourceFiles, cancellationToken);
            if (special) {
                return special;
            }
        }

        const checker = program.getTypeChecker();
        // constructors should use the class symbol, detected by name, if present
        const symbol = checker.getSymbolAtLocation(isConstructorDeclaration(node) && node.parent.name || node);

        // Could not find a symbol e.g. unknown identifier
        if (!symbol) {
            // String literal might be a property (and thus have a symbol), so do this here rather than in getReferencedSymbolsSpecial.
            if (!options.implementations && isStringLiteralLike(node)) {
                if (isModuleSpecifierLike(node)) {
                    const fileIncludeReasons = program.getFileIncludeReasons();
                    const referencedFileName = node.getSourceFile().resolvedModules?.get(node.text, getModeForUsageLocation(node.getSourceFile(), node))?.resolvedModule?.resolvedFileName;
                    const referencedFile = referencedFileName ? program.getSourceFile(referencedFileName) : undefined;
                    if (referencedFile) {
                        return [{ definition: { type: DefinitionKind.String, node }, references: getReferencesForNonModule(referencedFile, fileIncludeReasons, program) || emptyArray }];
                    }
                    // Fall through to string literal references. This is not very likely to return
                    // anything useful, but I guess it's better than nothing, and there's an existing
                    // test that expects this to happen (fourslash/cases/untypedModuleImport.ts).
                }
                return getReferencesForStringLiteral(node, sourceFiles, checker, cancellationToken);
            }
            return undefined;
        }

        if (symbol.escapedName === InternalSymbolName.ExportEquals) {
            return getReferencedSymbolsForModule(program, symbol.parent!, /*excludeImportTypeOfExportEquals*/ false, sourceFiles, sourceFilesSet);
        }

        const moduleReferences = getReferencedSymbolsForModuleIfDeclaredBySourceFile(symbol, program, sourceFiles, cancellationToken, options, sourceFilesSet);
        if (moduleReferences && !(symbol.flags & SymbolFlags.Transient)) {
            return moduleReferences;
        }

        const aliasedSymbol = getMergedAliasedSymbolOfNamespaceExportDeclaration(node, symbol, checker);
        const moduleReferencesOfExportTarget = aliasedSymbol &&
            getReferencedSymbolsForModuleIfDeclaredBySourceFile(aliasedSymbol, program, sourceFiles, cancellationToken, options, sourceFilesSet);

        const references = getReferencedSymbolsForSymbol(symbol, node, sourceFiles, sourceFilesSet, checker, cancellationToken, options);
        return mergeReferences(program, moduleReferences, references, moduleReferencesOfExportTarget);
    }

    export function getAdjustedNode(node: Node, options: Options) {
        if (options.use === FindReferencesUse.References) {
            node = getAdjustedReferenceLocation(node);
        }
        else if (options.use === FindReferencesUse.Rename) {
            node = getAdjustedRenameLocation(node);
        }
        return node;
    }

    export function getReferencesForFileName(fileName: string, program: Program, sourceFiles: readonly SourceFile[], sourceFilesSet: ReadonlySet<string> = new Set(sourceFiles.map(f => f.fileName))): readonly Entry[] {
        const moduleSymbol = program.getSourceFile(fileName)?.symbol;
        if (moduleSymbol) {
            return getReferencedSymbolsForModule(program, moduleSymbol, /*excludeImportTypeOfExportEquals*/ false, sourceFiles, sourceFilesSet)[0]?.references || emptyArray;
        }
        const fileIncludeReasons = program.getFileIncludeReasons();
        const referencedFile = program.getSourceFile(fileName);
        return referencedFile && fileIncludeReasons && getReferencesForNonModule(referencedFile, fileIncludeReasons, program) || emptyArray;
    }

    /**
     * Retrieves the span entries for a non-module referenced file.
     * @param referencedFile - The source file being referenced.
     * @param refFileMap - The map of reference file paths to their include reasons.
     * @param program - The program containing the source files.
     * @returns An array of span entries or undefined if none exist.
     */
    function getReferencesForNonModule(referencedFile: SourceFile, refFileMap: MultiMap<Path, FileIncludeReason>, program: Program): readonly SpanEntry[] | undefined {
        let entries: SpanEntry[] | undefined;
        const references = refFileMap.get(referencedFile.path) || emptyArray;
        for (const ref of references) {
            if (isReferencedFile(ref)) {
                const referencingFile = program.getSourceFileByPath(ref.file)!;
                const location = getReferencedFileLocation(program.getSourceFileByPath, ref);
                if (isReferenceFileLocation(location)) {
                    entries = append(entries, {
                        kind: EntryKind.Span,
                        fileName: referencingFile.fileName,
                        textSpan: createTextSpanFromRange(location)
                    });
                }
            }
        }
        return entries;
    }

    /**
     * Returns the merged aliased symbol of a namespace export declaration.
     * @param {Node} node - The node to check.
     * @param {Symbol} symbol - The symbol to check.
     * @param {TypeChecker} checker - The type checker to use.
     * @returns {Symbol|undefined} - The merged symbol or undefined if not found.
     */
    function getMergedAliasedSymbolOfNamespaceExportDeclaration(node: Node, symbol: Symbol, checker: TypeChecker) {
        if (node.parent && isNamespaceExportDeclaration(node.parent)) {
            const aliasedSymbol = checker.getAliasedSymbol(symbol);
            const targetSymbol = checker.getMergedSymbol(aliasedSymbol);
            if (aliasedSymbol !== targetSymbol) {
                return targetSymbol;
            }
        }
        return undefined;
    }

    /**
     * Retrieves referenced symbols for a module if it is declared by a source file.
     * @param symbol - The symbol to retrieve referenced symbols for.
     * @param program - The program containing the symbol.
     * @param sourceFiles - The source files to search for references.
     * @param cancellationToken - The cancellation token.
     * @param options - The options for retrieving references.
     * @param sourceFilesSet - The set of source files to search for references.
     * @returns The referenced symbols for the module if it is declared by a source file, otherwise undefined.
     */
    function getReferencedSymbolsForModuleIfDeclaredBySourceFile(symbol: Symbol, program: Program, sourceFiles: readonly SourceFile[], cancellationToken: CancellationToken, options: Options, sourceFilesSet: ReadonlySet<string>) {
        const moduleSourceFile = (symbol.flags & SymbolFlags.Module) && symbol.declarations && find(symbol.declarations, isSourceFile);
        if (!moduleSourceFile) return undefined;
        const exportEquals = symbol.exports!.get(InternalSymbolName.ExportEquals);
        // If !!exportEquals, we're about to add references to `import("mod")` anyway, so don't double-count them.
        const moduleReferences = getReferencedSymbolsForModule(program, symbol, !!exportEquals, sourceFiles, sourceFilesSet);
        if (!exportEquals || !sourceFilesSet.has(moduleSourceFile.fileName)) return moduleReferences;
        // Continue to get references to 'export ='.
        const checker = program.getTypeChecker();
        symbol = skipAlias(exportEquals, checker);
        return mergeReferences(program, moduleReferences, getReferencedSymbolsForSymbol(symbol, /*node*/ undefined, sourceFiles, sourceFilesSet, checker, cancellationToken, options));
    }

    /**
     * Merges an array of SymbolAndEntries objects by sorting them (by file index in sourceFiles and their location in it) that point to the same definition symbol.
     *
     * @param program - The Program instance.
     * @param referencesToMerge - An array of SymbolAndEntries objects to merge.
     *
     * @returns An array of SymbolAndEntries objects merged by sorting them.
     */
    function mergeReferences(program: Program, ...referencesToMerge: (SymbolAndEntries[] | undefined)[]): SymbolAndEntries[] | undefined {
        let result: SymbolAndEntries[] | undefined;
        for (const references of referencesToMerge) {
            if (!references || !references.length) continue;
            if (!result) {
                result = references;
                continue;
            }
            for (const entry of references) {
                if (!entry.definition || entry.definition.type !== DefinitionKind.Symbol) {
                    result.push(entry);
                    continue;
                }
                const symbol = entry.definition.symbol;
                const refIndex = findIndex(result, ref => !!ref.definition &&
                    ref.definition.type === DefinitionKind.Symbol &&
                    ref.definition.symbol === symbol);
                if (refIndex === -1) {
                    result.push(entry);
                    continue;
                }

                const reference = result[refIndex];
                result[refIndex] = {
                    definition: reference.definition,
                    references: reference.references.concat(entry.references).sort((entry1, entry2) => {
                        const entry1File = getSourceFileIndexOfEntry(program, entry1);
                        const entry2File = getSourceFileIndexOfEntry(program, entry2);
                        if (entry1File !== entry2File) {
                            return compareValues(entry1File, entry2File);
                        }

                        const entry1Span = getTextSpanOfEntry(entry1);
                        const entry2Span = getTextSpanOfEntry(entry2);
                        return entry1Span.start !== entry2Span.start ?
                            compareValues(entry1Span.start, entry2Span.start) :
                            compareValues(entry1Span.length, entry2Span.length);
                    })
                };
            }
        }
        return result;
    }

    function getSourceFileIndexOfEntry(program: Program, entry: Entry) {
        const sourceFile = entry.kind === EntryKind.Span ?
            program.getSourceFile(entry.fileName)! :
            entry.node.getSourceFile();
        return program.getSourceFiles().indexOf(sourceFile);
    }

    /**
     * Returns an array of SymbolAndEntries objects representing the references to a given symbol in a module.
     * @param program The Program object representing the TypeScript program.
     * @param symbol The Symbol object representing the symbol to find references for.
     * @param excludeImportTypeOfExportEquals A boolean indicating whether to exclude import types of export equals.
     * @param sourceFiles An array of SourceFile objects representing the source files to search for references in.
     * @param sourceFilesSet A ReadonlySet of strings representing the set of source file names to search for references in.
     * @returns An array of SymbolAndEntries objects representing the references to the given symbol in the module.
     */
    function getReferencedSymbolsForModule(program: Program, symbol: Symbol, excludeImportTypeOfExportEquals: boolean, sourceFiles: readonly SourceFile[], sourceFilesSet: ReadonlySet<string>): SymbolAndEntries[] {
        Debug.assert(!!symbol.valueDeclaration);

        const references = mapDefined<ModuleReference, Entry>(findModuleReferences(program, sourceFiles, symbol), reference => {
            if (reference.kind === "import") {
                const parent = reference.literal.parent;
                if (isLiteralTypeNode(parent)) {
                    const importType = cast(parent.parent, isImportTypeNode);
                    if (excludeImportTypeOfExportEquals && !importType.qualifier) {
                        return undefined;
                    }
                }
                // import("foo") with no qualifier will reference the `export =` of the module, which may be referenced anyway.
                return nodeEntry(reference.literal);
            }
            else if (reference.kind === "implicit") {
                // Return either: The first JSX node in the (if not a tslib import), the first statement of the file, or the whole file if neither of those exist
                const range = reference.literal.text !== externalHelpersModuleNameText && forEachChildRecursively(
                    reference.referencingFile,
                    n => !(n.transformFlags & TransformFlags.ContainsJsx) ? "skip" : isJsxElement(n) || isJsxSelfClosingElement(n) || isJsxFragment(n) ? n : undefined
                ) || reference.referencingFile.statements[0] || reference.referencingFile;
                return nodeEntry(range);
            }
            else {
                return {
                    kind: EntryKind.Span,
                    fileName: reference.referencingFile.fileName,
                    textSpan: createTextSpanFromRange(reference.ref),
                };
            }
        });

        if (symbol.declarations) {
            for (const decl of symbol.declarations) {
                switch (decl.kind) {
                    case SyntaxKind.SourceFile:
                        // Don't include the source file itself. (This may not be ideal behavior, but awkward to include an entire file as a reference.)
                        break;
                    case SyntaxKind.ModuleDeclaration:
                        if (sourceFilesSet.has(decl.getSourceFile().fileName)) {
                            references.push(nodeEntry((decl as ModuleDeclaration).name));
                        }
                        break;
                    default:
                        // This may be merged with something.
                        Debug.assert(!!(symbol.flags & SymbolFlags.Transient), "Expected a module symbol to be declared by a SourceFile or ModuleDeclaration.");
                }
            }
        }

        const exported = symbol.exports!.get(InternalSymbolName.ExportEquals);
        if (exported?.declarations) {
            for (const decl of exported.declarations) {
                const sourceFile = decl.getSourceFile();
                if (sourceFilesSet.has(sourceFile.fileName)) {
                    // At `module.exports = ...`, reference node is `module`
                    const node = isBinaryExpression(decl) && isPropertyAccessExpression(decl.left) ? decl.left.expression :
                        isExportAssignment(decl) ? Debug.checkDefined(findChildOfKind(decl, SyntaxKind.ExportKeyword, sourceFile)) :
                        getNameOfDeclaration(decl) || decl;
                    references.push(nodeEntry(node));
                }
            }
        }

        return references.length ? [{ definition: { type: DefinitionKind.Symbol, symbol }, references }] : emptyArray;
    }

    /** As in a `readonly prop: any` or `constructor(readonly prop: any)`, not a `readonly any[]`. */
    function isReadonlyTypeOperator(node: Node): boolean {
        return node.kind === SyntaxKind.ReadonlyKeyword
            && isTypeOperatorNode(node.parent)
            && node.parent.operator === SyntaxKind.ReadonlyKeyword;
    }

    /**
     * Retrieves referenced symbols for special node kinds.
     * @param node The node to retrieve referenced symbols for.
     * @param sourceFiles An array of source files to search for references in.
     * @param cancellationToken A token used to cancel the operation if needed.
     * @returns An array of SymbolAndEntries objects representing the referenced symbols and their entries, or undefined if no referenced symbols were found.
     */
    function getReferencedSymbolsSpecial(node: Node, sourceFiles: readonly SourceFile[], cancellationToken: CancellationToken): SymbolAndEntries[] | undefined {
        if (isTypeKeyword(node.kind)) {
            // A void expression (i.e., `void foo()`) is not special, but the `void` type is.
            if (node.kind === SyntaxKind.VoidKeyword && isVoidExpression(node.parent)) {
                return undefined;
            }

            // A modifier readonly (like on a property declaration) is not special;
            // a readonly type keyword (like `readonly string[]`) is.
            if (node.kind === SyntaxKind.ReadonlyKeyword && !isReadonlyTypeOperator(node)) {
                return undefined;
            }
            // Likewise, when we *are* looking for a special keyword, make sure we
            // *don't* include readonly member modifiers.
            return getAllReferencesForKeyword(
                sourceFiles,
                node.kind,
                cancellationToken,
                node.kind === SyntaxKind.ReadonlyKeyword ? isReadonlyTypeOperator : undefined);
        }

        if (isImportMeta(node.parent) && node.parent.name === node) {
            return getAllReferencesForImportMeta(sourceFiles, cancellationToken);
        }

        if (isStaticModifier(node) && isClassStaticBlockDeclaration(node.parent)) {
            return [{ definition: { type: DefinitionKind.Keyword, node }, references: [nodeEntry(node)] }];
        }

        // Labels
        if (isJumpStatementTarget(node)) {
            const labelDefinition = getTargetLabel(node.parent, node.text);
            // if we have a label definition, look within its statement for references, if not, then
            // the label is undefined and we have no results..
            return labelDefinition && getLabelReferencesInNode(labelDefinition.parent, labelDefinition);
        }
        else if (isLabelOfLabeledStatement(node)) {
            // it is a label definition and not a target, search within the parent labeledStatement
            return getLabelReferencesInNode(node.parent, node);
        }

        if (isThis(node)) {
            return getReferencesForThisKeyword(node, sourceFiles, cancellationToken);
        }

        if (node.kind === SyntaxKind.SuperKeyword) {
            return getReferencesForSuperKeyword(node);
        }

        return undefined;
    }

    /**
     * Finds all references to a given symbol within a set of source files.
     * @param originalSymbol - The symbol to search for references to.
     * @param node - The node to search within. If undefined, searches all nodes.
     * @param sourceFiles - The set of source files to search within.
     * @param sourceFilesSet - A set of source file names to limit the search to.
     * @param checker - The type checker to use for symbol resolution.
     * @param cancellationToken - A cancellation token to cancel the search.
     * @param options - Options for the search, such as whether to include implementations or not.
     * @returns An array of SymbolAndEntries objects representing the references found.
     */
    function getReferencedSymbolsForSymbol(originalSymbol: Symbol, node: Node | undefined, sourceFiles: readonly SourceFile[], sourceFilesSet: ReadonlySet<string>, checker: TypeChecker, cancellationToken: CancellationToken, options: Options): SymbolAndEntries[] {
        const symbol = node && skipPastExportOrImportSpecifierOrUnion(originalSymbol, node, checker, /*useLocalSymbolForExportSpecifier*/ !isForRenameWithPrefixAndSuffixText(options)) || originalSymbol;

        // Compute the meaning from the location and the symbol it references
        const searchMeaning = node ? getIntersectingMeaningFromDeclarations(node, symbol) : SemanticMeaning.All;
        const result: SymbolAndEntries[] = [];
        const state = new State(sourceFiles, sourceFilesSet, node ? getSpecialSearchKind(node) : SpecialSearchKind.None, checker, cancellationToken, searchMeaning, options, result);

        const exportSpecifier = !isForRenameWithPrefixAndSuffixText(options) || !symbol.declarations ? undefined : find(symbol.declarations, isExportSpecifier);
        if (exportSpecifier) {
            // When renaming at an export specifier, rename the export and not the thing being exported.
            getReferencesAtExportSpecifier(exportSpecifier.name, symbol, exportSpecifier, state.createSearch(node, originalSymbol, /*comingFrom*/ undefined), state, /*addReferencesHere*/ true, /*alwaysGetReferences*/ true);
        }
        else if (node && node.kind === SyntaxKind.DefaultKeyword && symbol.escapedName === InternalSymbolName.Default && symbol.parent) {
            addReference(node, symbol, state);
            searchForImportsOfExport(node, symbol, { exportingModuleSymbol: symbol.parent, exportKind: ExportKind.Default }, state);
        }
        else {
            const search = state.createSearch(node, symbol, /*comingFrom*/ undefined, { allSearchSymbols: node ? populateSearchSymbolSet(symbol, node, checker, options.use === FindReferencesUse.Rename, !!options.providePrefixAndSuffixTextForRename, !!options.implementations) : [symbol] });
            getReferencesInContainerOrFiles(symbol, state, search);
        }

        return result;
    }

    /**
     * Searches for references of a given symbol in a container or files.
     * @param {Symbol} symbol - The symbol to search for references of.
     * @param {State} state - The state object containing source files and cancellation token.
     * @param {Search} search - The search object containing the name to search for and search type.
     * @returns {void}
     */
    function getReferencesInContainerOrFiles(symbol: Symbol, state: State, search: Search): void {
        // Try to get the smallest valid scope that we can limit our search to;
        // otherwise we'll need to search globally (i.e. include each file).
        const scope = getSymbolScope(symbol);
        if (scope) {
            getReferencesInContainer(scope, scope.getSourceFile(), search, state, /*addReferencesHere*/ !(isSourceFile(scope) && !contains(state.sourceFiles, scope)));
        }
        else {
            // Global search
            for (const sourceFile of state.sourceFiles) {
                state.cancellationToken.throwIfCancellationRequested();
                searchForName(sourceFile, search, state);
            }
        }
    }

    /**
     * Returns the SpecialSearchKind of a given Node.
     * @param {Node} node - The Node to check.
     * @returns {SpecialSearchKind} - The SpecialSearchKind of the Node.
     */
    function getSpecialSearchKind(node: Node): SpecialSearchKind {
        switch (node.kind) {
            case SyntaxKind.Constructor:
            case SyntaxKind.ConstructorKeyword:
                return SpecialSearchKind.Constructor;
            case SyntaxKind.Identifier:
                if (isClassLike(node.parent)) {
                    Debug.assert(node.parent.name === node);
                    return SpecialSearchKind.Class;
                }
                // falls through
            default:
                return SpecialSearchKind.None;
        }
    }

    /**
     * Retrieves the symbol for a given node, skipping past any export or import specifiers or union types.
     * @param symbol - The symbol to retrieve.
     * @param node - The node to retrieve the symbol from.
     * @param checker - The TypeChecker to use.
     * @param useLocalSymbolForExportSpecifier - Whether to use the local symbol for export specifiers.
     * @returns The retrieved symbol, or undefined if none was found.
     */
    function skipPastExportOrImportSpecifierOrUnion(symbol: Symbol, node: Node, checker: TypeChecker, useLocalSymbolForExportSpecifier: boolean): Symbol | undefined {
        const { parent } = node;
        if (isExportSpecifier(parent) && useLocalSymbolForExportSpecifier) {
            return getLocalSymbolForExportSpecifier(node as Identifier, symbol, parent, checker);
        }
        // If the symbol is declared as part of a declaration like `{ type: "a" } | { type: "b" }`, use the property on the union type to get more references.
        return firstDefined(symbol.declarations, decl => {
            if (!decl.parent) {
                // Ignore UMD module and global merge
                if (symbol.flags & SymbolFlags.Transient) return undefined;
                // Assertions for GH#21814. We should be handling SourceFile symbols in `getReferencedSymbolsForModule` instead of getting here.
                Debug.fail(`Unexpected symbol at ${Debug.formatSyntaxKind(node.kind)}: ${Debug.formatSymbol(symbol)}`);
            }
            return isTypeLiteralNode(decl.parent) && isUnionTypeNode(decl.parent.parent)
                ? checker.getPropertyOfType(checker.getTypeFromTypeNode(decl.parent.parent), symbol.name)
                : undefined;
        });
    }

    /**
     * Represents a search for a symbol within a program.
     * @interface
     * @property {ImportExport} [comingFrom] - If coming from an export, we will not recursively search for the imported symbol (since that's where we came from).
     * @property {Symbol} symbol - The symbol being searched for.
     * @property {string} text - The text of the symbol being searched for.
     * @property {__String} escapedText - The escaped text of the symbol being searched for.
     * @property {readonly Symbol[] | undefined} parents - Only set if `options.implementations` is true. These are the symbols checked to get the implementations of a property access.
     * @property {readonly Symbol[]} allSearchSymbols - An array of all symbols being searched for.
     * @function includes - Whether a symbol is in the search set. Do not compare directly to `symbol` because there may be related symbols to search for. See `populateSearchSymbolSet`.
     */
    interface Search {
        /** If coming from an export, we will not recursively search for the imported symbol (since that's where we came from). */
        readonly comingFrom?: ImportExport;

        readonly symbol: Symbol;
        readonly text: string;
        readonly escapedText: __String;
        /** Only set if `options.implementations` is true. These are the symbols checked to get the implementations of a property access. */
        readonly parents: readonly Symbol[] | undefined;
        readonly allSearchSymbols: readonly Symbol[];

        /**
         * Whether a symbol is in the search set.
         * Do not compare directly to `symbol` because there may be related symbols to search for. See `populateSearchSymbolSet`.
         */
        includes(symbol: Symbol): boolean;
    }

    const enum SpecialSearchKind {
        None,
        Constructor,
        Class,
    }

    function getNonModuleSymbolOfMergedModuleSymbol(symbol: Symbol) {
        if (!(symbol.flags & (SymbolFlags.Module | SymbolFlags.Transient))) return undefined;
        const decl = symbol.declarations && find(symbol.declarations, d => !isSourceFile(d) && !isModuleDeclaration(d));
        return decl && decl.symbol;
    }

    /**
     * Holds all state needed for finding references.
     * @class
     */
    class State {
        /** Cache for `explicitlyinheritsFrom`. */
        readonly inheritsFromCache = new Map<string, boolean>();

        /**
         * Type nodes can contain multiple references to the same type. For example:
         *      let x: Foo & (Foo & Bar) = ...
         * Because we are returning the implementation locations and not the identifier locations,
         * duplicate entries would be returned here as each of the type references is part of
         * the same implementation. For that reason, check before we add a new entry.
         */
        readonly markSeenContainingTypeReference = nodeSeenTracker();

        /**
         * It's possible that we will encounter the right side of `export { foo as bar } from "x";` more than once.
         * For example:
         *     // b.ts
         *     export { foo as bar } from "./a";
         *     import { bar } from "./b";
         *
         * Normally at `foo as bar` we directly add `foo` and do not locally search for it (since it doesn't declare a local).
         * But another reference to it may appear in the same source file.
         * See `tests/cases/fourslash/transitiveExportImports3.ts`.
         */
        readonly markSeenReExportRHS = nodeSeenTracker();

        constructor(
            readonly sourceFiles: readonly SourceFile[],
            readonly sourceFilesSet: ReadonlySet<string>,
            readonly specialSearchKind: SpecialSearchKind,
            readonly checker: TypeChecker,
            readonly cancellationToken: CancellationToken,
            readonly searchMeaning: SemanticMeaning,
            readonly options: Options,
            private readonly result: SymbolAndEntries[]) {
        }

        includesSourceFile(sourceFile: SourceFile): boolean {
            return this.sourceFilesSet.has(sourceFile.fileName);
        }

        private importTracker: ImportTracker | undefined;
        /** Gets every place to look for references of an exported symbols. See `ImportsResult` in `importTracker.ts` for more documentation. */
        getImportSearches(exportSymbol: Symbol, exportInfo: ExportInfo): ImportsResult {
            if (!this.importTracker) this.importTracker = createImportTracker(this.sourceFiles, this.sourceFilesSet, this.checker, this.cancellationToken);
            return this.importTracker(exportSymbol, exportInfo, this.options.use === FindReferencesUse.Rename);
        }

        /**
         * Creates a Search object based on the provided parameters.
         * @param location The location of the search.
         * @param symbol The symbol to search for.
         * @param comingFrom The import/export statement the symbol is coming from.
         * @param searchOptions Optional search options.
         * @param searchOptions.text The text to search for.
         * @param searchOptions.allSearchSymbols Set of additional symbols for use by `includes`.
         * @returns The created Search object.
         */
        createSearch(location: Node | undefined, symbol: Symbol, comingFrom: ImportExport | undefined, searchOptions: { text?: string, allSearchSymbols?: Symbol[] } = {}): Search {
            // Note: if this is an external module symbol, the name doesn't include quotes.
            // Note: getLocalSymbolForExportDefault handles `export default class C {}`, but not `export default C` or `export { C as default }`.
            // The other two forms seem to be handled downstream (e.g. in `skipPastExportOrImportSpecifier`), so special-casing the first form
            // here appears to be intentional).
            const {
                text = stripQuotes(symbolName(getLocalSymbolForExportDefault(symbol) || getNonModuleSymbolOfMergedModuleSymbol(symbol) || symbol)),
                allSearchSymbols = [symbol],
            } = searchOptions;
            const escapedText = escapeLeadingUnderscores(text);
            const parents = this.options.implementations && location ? getParentSymbolsOfPropertyAccess(location, symbol, this.checker) : undefined;
            return { symbol, comingFrom, text, escapedText, parents, allSearchSymbols, includes: sym => contains(allSearchSymbols, sym) };
        }

        private readonly symbolIdToReferences: Entry[][] = [];
        /**
         * Callback to add references for a particular searched symbol.
         * This initializes a reference group, so only call this if you will add at least one reference.
         */
        referenceAdder(searchSymbol: Symbol): (node: Node, kind?: NodeEntryKind) => void {
            const symbolId = getSymbolId(searchSymbol);
            let references = this.symbolIdToReferences[symbolId];
            if (!references) {
                references = this.symbolIdToReferences[symbolId] = [];
                this.result.push({ definition: { type: DefinitionKind.Symbol, symbol: searchSymbol }, references });
            }
            return (node, kind) => references.push(nodeEntry(node, kind));
        }

        /** Add a reference with no associated definition. */
        addStringOrCommentReference(fileName: string, textSpan: TextSpan): void {
            this.result.push({
                definition: undefined,
                references: [{ kind: EntryKind.Span, fileName, textSpan }]
            });
        }

        // Source file ID -> symbol ID -> Whether the symbol has been searched for in the source file.
        private readonly sourceFileToSeenSymbols: Set<number>[] = [];
        /**
         * Marks symbols as searched in a source file and returns a boolean indicating if any new symbols were added to the set.
         * @param {SourceFile} sourceFile - The source file to mark symbols as searched in.
         * @param {readonly Symbol[]} symbols - The symbols to mark as searched.
         * @returns {boolean} - A boolean indicating if any new symbols were added to the set.
         */
        markSearchedSymbols(sourceFile: SourceFile, symbols: readonly Symbol[]): boolean {
            const sourceId = getNodeId(sourceFile);
            const seenSymbols = this.sourceFileToSeenSymbols[sourceId] || (this.sourceFileToSeenSymbols[sourceId] = new Set<number>());

            let anyNewSymbols = false;
            for (const sym of symbols) {
                anyNewSymbols = tryAddToSet(seenSymbols, getSymbolId(sym)) || anyNewSymbols;
            }
            return anyNewSymbols;
        }
    }

    /**
     * Searches for all imports of a given exported symbol using `State.getImportSearches`.
     * @param exportLocation - The location of the exported symbol.
     * @param exportSymbol - The exported symbol to search for imports of.
     * @param exportInfo - Information about the exported symbol.
     * @param state - The state object containing information about the program being analyzed.
     */
    function searchForImportsOfExport(exportLocation: Node, exportSymbol: Symbol, exportInfo: ExportInfo, state: State): void {
        const { importSearches, singleReferences, indirectUsers } = state.getImportSearches(exportSymbol, exportInfo);

        // For `import { foo as bar }` just add the reference to `foo`, and don't otherwise search in the file.
        if (singleReferences.length) {
            const addRef = state.referenceAdder(exportSymbol);
            for (const singleRef of singleReferences) {
                if (shouldAddSingleReference(singleRef, state)) addRef(singleRef);
            }
        }

        // For each import, find all references to that import in its source file.
        for (const [importLocation, importSymbol] of importSearches) {
            getReferencesInSourceFile(importLocation.getSourceFile(), state.createSearch(importLocation, importSymbol, ImportExport.Export), state);
        }

        if (indirectUsers.length) {
            let indirectSearch: Search | undefined;
            switch (exportInfo.exportKind) {
                case ExportKind.Named:
                    indirectSearch = state.createSearch(exportLocation, exportSymbol, ImportExport.Export);
                    break;
                case ExportKind.Default:
                    // Search for a property access to '.default'. This can't be renamed.
                    indirectSearch = state.options.use === FindReferencesUse.Rename ? undefined : state.createSearch(exportLocation, exportSymbol, ImportExport.Export, { text: "default" });
                    break;
                case ExportKind.ExportEquals:
                    break;
            }
            if (indirectSearch) {
                for (const indirectUser of indirectUsers) {
                    searchForName(indirectUser, indirectSearch, state);
                }
            }
        }
    }

    /**
     * Iterates over all references to a specific export in a set of source files and calls a callback function for each reference.
     * @param sourceFiles - An array of source files to search for references.
     * @param checker - The type checker to use for symbol resolution.
     * @param cancellationToken - An optional cancellation token.
     * @param exportSymbol - The symbol of the export to search for references to.
     * @param exportingModuleSymbol - The symbol of the module that is exporting the exportSymbol.
     * @param exportName - The name of the export to search for references to.
     * @param isDefaultExport - A boolean indicating whether the export is a default export.
     * @param cb - The callback function to call for each reference found.
     */
    export function eachExportReference(
        sourceFiles: readonly SourceFile[],
        checker: TypeChecker,
        cancellationToken: CancellationToken | undefined,
        exportSymbol: Symbol,
        exportingModuleSymbol: Symbol,
        exportName: string,
        isDefaultExport: boolean,
        cb: (ref: Identifier) => void,
    ): void {
        const importTracker = createImportTracker(sourceFiles, new Set(sourceFiles.map(f => f.fileName)), checker, cancellationToken);
        const { importSearches, indirectUsers, singleReferences } = importTracker(exportSymbol, { exportKind: isDefaultExport ? ExportKind.Default : ExportKind.Named, exportingModuleSymbol }, /*isForRename*/ false);
        for (const [importLocation] of importSearches) {
            cb(importLocation);
        }
        for (const singleReference of singleReferences) {
            if (isIdentifier(singleReference) && isImportTypeNode(singleReference.parent)) {
                cb(singleReference);
            }
        }
        for (const indirectUser of indirectUsers) {
            for (const node of getPossibleSymbolReferenceNodes(indirectUser, isDefaultExport ? "default" : exportName)) {
                // Import specifiers should be handled by importSearches
                const symbol = checker.getSymbolAtLocation(node);
                const hasExportAssignmentDeclaration = some(symbol?.declarations, d => tryCast(d, isExportAssignment) ? true : false);
                if (isIdentifier(node) && !isImportOrExportSpecifier(node.parent) && (symbol === exportSymbol || hasExportAssignmentDeclaration)) {
                    cb(node);
                }
            }
        }
    }

    function shouldAddSingleReference(singleRef: Identifier | StringLiteral, state: State): boolean {
        if (!hasMatchingMeaning(singleRef, state)) return false;
        if (state.options.use !== FindReferencesUse.Rename) return true;
        // Don't rename an import type `import("./module-name")` when renaming `name` in `export = name;`
        if (!isIdentifier(singleRef)) return false;
        // At `default` in `import { default as x }` or `export { default as x }`, do add a reference, but do not rename.
        return !(isImportOrExportSpecifier(singleRef.parent) && singleRef.escapedText === InternalSymbolName.Default);
    }

    // Go to the symbol we imported from and find references for it.
    function searchForImportedSymbol(symbol: Symbol, state: State): void {
        if (!symbol.declarations) return;

        for (const declaration of symbol.declarations) {
            const exportingFile = declaration.getSourceFile();
            // Need to search in the file even if it's not in the search-file set, because it might export the symbol.
            getReferencesInSourceFile(exportingFile, state.createSearch(declaration, symbol, ImportExport.Import), state, state.includesSourceFile(exportingFile));
        }
    }

    /** Search for all occurrences of an identifier in a source file (and filter out the ones that match). */
    function searchForName(sourceFile: SourceFile, search: Search, state: State): void {
        if (getNameTable(sourceFile).get(search.escapedText) !== undefined) {
            getReferencesInSourceFile(sourceFile, search, state);
        }
    }

    function getPropertySymbolOfDestructuringAssignment(location: Node, checker: TypeChecker): Symbol | undefined {
        return isArrayLiteralOrObjectLiteralDestructuringPattern(location.parent.parent)
            ? checker.getPropertySymbolOfDestructuringAssignment(location as Identifier)
            : undefined;
    }

    /**
     * Determines the smallest scope in which a symbol may have named references.
     * Note that not every construct has been accounted for. This function can
     * probably be improved.
     *
     * @param symbol - The symbol to determine the scope for.
     * @returns If the scope cannot be determined, returns undefined, implying that
     * a reference to a symbol can occur anywhere. Otherwise, returns the Node representing
     * the smallest scope in which the symbol may have named references.
     */
    function getSymbolScope(symbol: Symbol): Node | undefined {
        // If this is the symbol of a named function expression or named class expression,
        // then named references are limited to its own scope.
        const { declarations, flags, parent, valueDeclaration } = symbol;
        if (valueDeclaration && (valueDeclaration.kind === SyntaxKind.FunctionExpression || valueDeclaration.kind === SyntaxKind.ClassExpression)) {
            return valueDeclaration;
        }

        if (!declarations) {
            return undefined;
        }

        // If this is private property or method, the scope is the containing class
        if (flags & (SymbolFlags.Property | SymbolFlags.Method)) {
            const privateDeclaration = find(declarations, d => hasEffectiveModifier(d, ModifierFlags.Private) || isPrivateIdentifierClassElementDeclaration(d));
            if (privateDeclaration) {
                return getAncestor(privateDeclaration, SyntaxKind.ClassDeclaration);
            }
            // Else this is a public property and could be accessed from anywhere.
            return undefined;
        }

        // If symbol is of object binding pattern element without property name we would want to
        // look for property too and that could be anywhere
        if (declarations.some(isObjectBindingElementWithoutPropertyName)) {
            return undefined;
        }

        /*
        If the symbol has a parent, it's globally visible unless:
        - It's a private property (handled above).
        - It's a type parameter.
        - The parent is an external module: then we should only search in the module (and recurse on the export later).
        - But if the parent has `export as namespace`, the symbol is globally visible through that namespace.
        */
        const exposedByParent = parent && !(symbol.flags & SymbolFlags.TypeParameter);
        if (exposedByParent && !(isExternalModuleSymbol(parent) && !parent.globalExports)) {
            return undefined;
        }

        let scope: Node | undefined;
        for (const declaration of declarations) {
            const container = getContainerNode(declaration);
            if (scope && scope !== container) {
                // Different declarations have different containers, bail out
                return undefined;
            }

            if (!container || container.kind === SyntaxKind.SourceFile && !isExternalOrCommonJsModule(container as SourceFile)) {
                // This is a global variable and not an external module, any declaration defined
                // within this scope is visible outside the file
                return undefined;
            }

            scope = container;
            if (isFunctionExpression(scope)) {
                let next: Node | undefined;
                while (next = getNextJSDocCommentLocation(scope)) {
                    scope = next;
                }
            }
        }

        // If symbol.parent, this means we are in an export of an external module. (Otherwise we would have returned `undefined` above.)
        // For an export of a module, we may be in a declaration file, and it may be accessed elsewhere. E.g.:
        //     declare module "a" { export type T = number; }
        //     declare module "b" { import { T } from "a"; export const x: T; }
        // So we must search the whole source file. (Because we will mark the source file as seen, we we won't return to it when searching for imports.)
        return exposedByParent ? scope!.getSourceFile() : scope; // TODO: GH#18217
    }

    /** Used as a quick check for whether a symbol is used at all in a file (besides its definition). */
    export function isSymbolReferencedInFile(definition: Identifier, checker: TypeChecker, sourceFile: SourceFile, searchContainer: Node = sourceFile): boolean {
        return eachSymbolReferenceInFile(definition, checker, sourceFile, () => true, searchContainer) || false;
    }

    /**
     * Iterates over each possible symbol reference node in a source file and invokes a callback function for each matching identifier token.
     * @template T
     * @param {Identifier} definition - The identifier token to search for.
     * @param {TypeChecker} checker - The type checker instance to use.
     * @param {SourceFile} sourceFile - The source file to search in.
     * @param {(token: Identifier) => T} cb - The callback function to invoke for each matching identifier token.
     * @param {Node} [searchContainer=sourceFile] - The node to start the search from.
     * @returns {T | undefined} The result of the callback function or undefined if no matching identifier token is found.
     */
    export function eachSymbolReferenceInFile<T>(definition: Identifier, checker: TypeChecker, sourceFile: SourceFile, cb: (token: Identifier) => T, searchContainer: Node = sourceFile): T | undefined {
        const symbol = isParameterPropertyDeclaration(definition.parent, definition.parent.parent)
            ? first(checker.getSymbolsOfParameterPropertyDeclaration(definition.parent, definition.text))
            : checker.getSymbolAtLocation(definition);
        if (!symbol) return undefined;
        for (const token of getPossibleSymbolReferenceNodes(sourceFile, symbol.name, searchContainer)) {
            if (!isIdentifier(token) || token === definition || token.escapedText !== definition.escapedText) continue;
            const referenceSymbol = checker.getSymbolAtLocation(token)!;
            if (referenceSymbol === symbol
                || checker.getShorthandAssignmentValueSymbol(token.parent) === symbol
                || isExportSpecifier(token.parent) && getLocalSymbolForExportSpecifier(token, referenceSymbol, token.parent, checker) === symbol) {
                const res = cb(token);
                if (res) return res;
            }
        }
    }

    /**
     * Returns an array of the top-most declaration nodes with the given name in the provided source file.
     * @param {string} declarationName - The name of the declaration to search for.
     * @param {SourceFile} sourceFile - The source file to search in.
     * @returns {readonly Node[]} - An array of the top-most declaration nodes with the given name.
     */
    export function getTopMostDeclarationNamesInFile(declarationName: string, sourceFile: SourceFile): readonly Node[] {
        const candidates = filter(getPossibleSymbolReferenceNodes(sourceFile, declarationName), name => !!getDeclarationFromName(name));
        return candidates.reduce((topMost, decl) => {
            const depth = getDepth(decl);
            if (!some(topMost.declarationNames) || depth === topMost.depth) {
                topMost.declarationNames.push(decl);
                topMost.depth = depth;
            }
            else if (depth < topMost.depth) {
                topMost.declarationNames = [decl];
                topMost.depth = depth;
            }
            return topMost;
        }, { depth: Infinity, declarationNames: [] as Node[] }).declarationNames;

        function getDepth(declaration: Node | undefined) {
            let depth = 0;
            while (declaration) {
                declaration = getContainerNode(declaration);
                depth++;
            }
            return depth;
        }
    }

    /**
     * Checks if a given SignatureDeclaration is used in any of the provided SourceFiles.
     * @param signature - The SignatureDeclaration to check.
     * @param sourceFiles - An array of SourceFiles to search for usage of the SignatureDeclaration.
     * @param checker - The TypeChecker to use for symbol resolution.
     * @param cb - A callback function to execute for each usage of the SignatureDeclaration.
     * @returns Returns a boolean indicating whether the SignatureDeclaration is used in any of the provided SourceFiles.
     */
    export function someSignatureUsage(
        signature: SignatureDeclaration,
        sourceFiles: readonly SourceFile[],
        checker: TypeChecker,
        cb: (name: Identifier, call?: CallExpression) => boolean
    ): boolean {
        if (!signature.name || !isIdentifier(signature.name)) return false;

        const symbol = Debug.checkDefined(checker.getSymbolAtLocation(signature.name));

        for (const sourceFile of sourceFiles) {
            for (const name of getPossibleSymbolReferenceNodes(sourceFile, symbol.name)) {
                if (!isIdentifier(name) || name === signature.name || name.escapedText !== signature.name.escapedText) continue;
                const called = climbPastPropertyAccess(name);
                const call = isCallExpression(called.parent) && called.parent.expression === called ? called.parent : undefined;
                const referenceSymbol = checker.getSymbolAtLocation(name);
                if (referenceSymbol && checker.getRootSymbols(referenceSymbol).some(s => s === symbol)) {
                    if (cb(name, call)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    function getPossibleSymbolReferenceNodes(sourceFile: SourceFile, symbolName: string, container: Node = sourceFile): readonly Node[] {
        return mapDefined(getPossibleSymbolReferencePositions(sourceFile, symbolName, container), pos => {
            const referenceLocation = getTouchingPropertyName(sourceFile, pos);
            return referenceLocation === sourceFile ? undefined : referenceLocation;
        });
    }

    /**
     * Returns an array of possible symbol reference positions in a given source file for a given symbol name.
     * @param sourceFile - The source file to search in.
     * @param symbolName - The name of the symbol to search for.
     * @param container - The node to search within. Defaults to the entire source file.
     * @returns An array of numbers representing the positions of possible symbol references.
     * @remarks This function is resilient in the face of a symbol with no name or zero length name. It also checks if the symbol is part of a larger word before adding it to the result array.
     */
    function getPossibleSymbolReferencePositions(sourceFile: SourceFile, symbolName: string, container: Node = sourceFile): readonly number[] {
        const positions: number[] = [];

        /// TODO: Cache symbol existence for files to save text search
        // Also, need to make this work for unicode escapes.

        // Be resilient in the face of a symbol with no name or zero length name
        if (!symbolName || !symbolName.length) {
            return positions;
        }

        const text = sourceFile.text;
        const sourceLength = text.length;
        const symbolNameLength = symbolName.length;

        let position = text.indexOf(symbolName, container.pos);
        while (position >= 0) {
            // If we are past the end, stop looking
            if (position > container.end) break;

            // We found a match.  Make sure it's not part of a larger word (i.e. the char
            // before and after it have to be a non-identifier char).
            const endPosition = position + symbolNameLength;

            if ((position === 0 || !isIdentifierPart(text.charCodeAt(position - 1), ScriptTarget.Latest)) &&
                (endPosition === sourceLength || !isIdentifierPart(text.charCodeAt(endPosition), ScriptTarget.Latest))) {
                // Found a real match.  Keep searching.
                positions.push(position);
            }
            position = text.indexOf(symbolName, position + symbolNameLength + 1);
        }

        return positions;
    }

    function getLabelReferencesInNode(container: Node, targetLabel: Identifier): SymbolAndEntries[] {
        const sourceFile = container.getSourceFile();
        const labelName = targetLabel.text;
        const references = mapDefined(getPossibleSymbolReferenceNodes(sourceFile, labelName, container), node =>
            // Only pick labels that are either the target label, or have a target that is the target label
            node === targetLabel || (isJumpStatementTarget(node) && getTargetLabel(node, labelName) === targetLabel) ? nodeEntry(node) : undefined);
        return [{ definition: { type: DefinitionKind.Label, node: targetLabel }, references }];
    }

    /**
     * Determines if a given node is a valid reference position for a search symbol name.
     * @param {Node} node - The node to check.
     * @param {string} searchSymbolName - The symbol name to search for.
     * @returns {boolean} - True if the node is a valid reference position, false otherwise.
     */
    function isValidReferencePosition(node: Node, searchSymbolName: string): boolean {
        // Compare the length so we filter out strict superstrings of the symbol we are looking for
        switch (node.kind) {
            case SyntaxKind.PrivateIdentifier:
                if (isJSDocMemberName(node.parent)) {
                    return true;
                }
                // falls through I guess
            case SyntaxKind.Identifier:
                return (node as PrivateIdentifier | Identifier).text.length === searchSymbolName.length;
            case SyntaxKind.NoSubstitutionTemplateLiteral:
            case SyntaxKind.StringLiteral: {
                const str = node as StringLiteralLike;
                return (isLiteralNameOfPropertyDeclarationOrIndexAccess(str) || isNameOfModuleDeclaration(node) || isExpressionOfExternalModuleImportEqualsDeclaration(node) || (isCallExpression(node.parent) && isBindableObjectDefinePropertyCall(node.parent) && node.parent.arguments[1] === node)) &&
                    str.text.length === searchSymbolName.length;
            }

            case SyntaxKind.NumericLiteral:
                return isLiteralNameOfPropertyDeclarationOrIndexAccess(node as NumericLiteral) && (node as NumericLiteral).text.length === searchSymbolName.length;

            case SyntaxKind.DefaultKeyword:
                return "default".length === searchSymbolName.length;

            default:
                return false;
        }
    }

    /**
     * Returns an array of SymbolAndEntries objects representing all references to the "meta" property of the import.meta object in the provided source files.
     * @param {readonly SourceFile[]} sourceFiles - An array of SourceFile objects to search for references in.
     * @param {CancellationToken} cancellationToken - A CancellationToken object that can be used to cancel the search.
     * @returns {SymbolAndEntries[] | undefined} - An array of SymbolAndEntries objects representing all references to the "meta" property of the import.meta object in the provided source files, or undefined if no references were found.
     */
    function getAllReferencesForImportMeta(sourceFiles: readonly SourceFile[], cancellationToken: CancellationToken): SymbolAndEntries[] | undefined {
        const references = flatMap(sourceFiles, sourceFile => {
            cancellationToken.throwIfCancellationRequested();
            return mapDefined(getPossibleSymbolReferenceNodes(sourceFile, "meta", sourceFile), node => {
                const parent = node.parent;
                if (isImportMeta(parent)) {
                    return nodeEntry(parent);
                }
            });
        });
        return references.length ? [{ definition: { type: DefinitionKind.Keyword, node: references[0].node }, references }] : undefined;
    }

    /**
     * Retrieves all references to a specified keyword in an array of source files.
     * @param sourceFiles An array of source files to search through.
     * @param keywordKind The syntax kind of the keyword to search for.
     * @param cancellationToken A token used to cancel the search if needed.
     * @param filter An optional function used to filter the search results.
     * @returns An array of symbol and entry objects representing the keyword definition and its references, or undefined if no references were found.
     */
    function getAllReferencesForKeyword(sourceFiles: readonly SourceFile[], keywordKind: SyntaxKind, cancellationToken: CancellationToken, filter?: (node: Node) => boolean): SymbolAndEntries[] | undefined {
        const references = flatMap(sourceFiles, sourceFile => {
            cancellationToken.throwIfCancellationRequested();
            return mapDefined(getPossibleSymbolReferenceNodes(sourceFile, tokenToString(keywordKind)!, sourceFile), referenceLocation => {
                if (referenceLocation.kind === keywordKind && (!filter || filter(referenceLocation))) {
                    return nodeEntry(referenceLocation);
                }
            });
        });
        return references.length ? [{ definition: { type: DefinitionKind.Keyword, node: references[0].node }, references }] : undefined;
    }

    function getReferencesInSourceFile(sourceFile: SourceFile, search: Search, state: State, addReferencesHere = true): void {
        state.cancellationToken.throwIfCancellationRequested();
        return getReferencesInContainer(sourceFile, sourceFile, search, state, addReferencesHere);
    }

    /**
     * Search within node "container" for references for a search value, where the search value is defined as a
     * tuple of(searchSymbol, searchText, searchLocation, and searchMeaning).
     * searchLocation: a node where the search value
     */
    function getReferencesInContainer(container: Node, sourceFile: SourceFile, search: Search, state: State, addReferencesHere: boolean): void {
        if (!state.markSearchedSymbols(sourceFile, search.allSearchSymbols)) {
            return;
        }

        for (const position of getPossibleSymbolReferencePositions(sourceFile, search.text, container)) {
            getReferencesAtLocation(sourceFile, position, search, state, addReferencesHere);
        }
    }

    function hasMatchingMeaning(referenceLocation: Node, state: State): boolean {
        return !!(getMeaningFromLocation(referenceLocation) & state.searchMeaning);
    }

    function getReferencesAtLocation(sourceFile: SourceFile, position: number, search: Search, state: State, addReferencesHere: boolean): void {
        const referenceLocation = getTouchingPropertyName(sourceFile, position);

        if (!isValidReferencePosition(referenceLocation, search.text)) {
            // This wasn't the start of a token.  Check to see if it might be a
            // match in a comment or string if that's what the caller is asking
            // for.
            if (!state.options.implementations && (state.options.findInStrings && isInString(sourceFile, position) || state.options.findInComments && isInNonReferenceComment(sourceFile, position))) {
                // In the case where we're looking inside comments/strings, we don't have
                // an actual definition.  So just use 'undefined' here.  Features like
                // 'Rename' won't care (as they ignore the definitions), and features like
                // 'FindReferences' will just filter out these results.
                state.addStringOrCommentReference(sourceFile.fileName, createTextSpan(position, search.text.length));
            }

            return;
        }

        if (!hasMatchingMeaning(referenceLocation, state)) return;

        let referenceSymbol = state.checker.getSymbolAtLocation(referenceLocation);
        if (!referenceSymbol) {
            return;
        }

        const parent = referenceLocation.parent;
        if (isImportSpecifier(parent) && parent.propertyName === referenceLocation) {
            // This is added through `singleReferences` in ImportsResult. If we happen to see it again, don't add it again.
            return;
        }

        if (isExportSpecifier(parent)) {
            Debug.assert(referenceLocation.kind === SyntaxKind.Identifier);
            getReferencesAtExportSpecifier(referenceLocation as Identifier, referenceSymbol, parent, search, state, addReferencesHere);
            return;
        }

        const relatedSymbol = getRelatedSymbol(search, referenceSymbol, referenceLocation, state);
        if (!relatedSymbol) {
            getReferenceForShorthandProperty(referenceSymbol, search, state);
            return;
        }

        switch (state.specialSearchKind) {
            case SpecialSearchKind.None:
                if (addReferencesHere) addReference(referenceLocation, relatedSymbol, state);
                break;
            case SpecialSearchKind.Constructor:
                addConstructorReferences(referenceLocation, sourceFile, search, state);
                break;
            case SpecialSearchKind.Class:
                addClassStaticThisReferences(referenceLocation, search, state);
                break;
            default:
                Debug.assertNever(state.specialSearchKind);
        }

        // Use the parent symbol if the location is commonjs require syntax on javascript files only.
        if (isInJSFile(referenceLocation)
            && isBindingElement(referenceLocation.parent)
            && isVariableDeclarationInitializedToBareOrAccessedRequire(referenceLocation.parent.parent.parent)) {
            referenceSymbol = referenceLocation.parent.symbol;
            // The parent will not have a symbol if it's an ObjectBindingPattern (when destructuring is used).  In
            // this case, just skip it, since the bound identifiers are not an alias of the import.
            if (!referenceSymbol) return;
        }

        getImportOrExportReferences(referenceLocation, referenceSymbol, search, state);
    }

    /**
     * Searches for references at an export specifier.
     * @param {Identifier} referenceLocation - The location of the reference.
     * @param {Symbol} referenceSymbol - The symbol being referenced.
     * @param {ExportSpecifier} exportSpecifier - The export specifier being searched.
     * @param {Search} search - The search parameters.
     * @param {State} state - The state of the search.
     * @param {boolean} addReferencesHere - Whether to add references at the current location.
     * @param {boolean} [alwaysGetReferences] - Whether to always get references.
     */
    function getReferencesAtExportSpecifier(
        referenceLocation: Identifier,
        referenceSymbol: Symbol,
        exportSpecifier: ExportSpecifier,
        search: Search,
        state: State,
        addReferencesHere: boolean,
        alwaysGetReferences?: boolean,
    ): void {
        Debug.assert(!alwaysGetReferences || !!state.options.providePrefixAndSuffixTextForRename, "If alwaysGetReferences is true, then prefix/suffix text must be enabled");

        const { parent, propertyName, name } = exportSpecifier;
        const exportDeclaration = parent.parent;
        const localSymbol = getLocalSymbolForExportSpecifier(referenceLocation, referenceSymbol, exportSpecifier, state.checker);
        if (!alwaysGetReferences && !search.includes(localSymbol)) {
            return;
        }

        if (!propertyName) {
            // Don't rename at `export { default } from "m";`. (but do continue to search for imports of the re-export)
            if (!(state.options.use === FindReferencesUse.Rename && (name.escapedText === InternalSymbolName.Default))) {
                addRef();
            }
        }
        else if (referenceLocation === propertyName) {
            // For `export { foo as bar } from "baz"`, "`foo`" will be added from the singleReferences for import searches of the original export.
            // For `export { foo as bar };`, where `foo` is a local, so add it now.
            if (!exportDeclaration.moduleSpecifier) {
                addRef();
            }

            if (addReferencesHere && state.options.use !== FindReferencesUse.Rename && state.markSeenReExportRHS(name)) {
                addReference(name, Debug.checkDefined(exportSpecifier.symbol), state);
            }
        }
        else {
            if (state.markSeenReExportRHS(referenceLocation)) {
                addRef();
            }
        }

        // For `export { foo as bar }`, rename `foo`, but not `bar`.
        if (!isForRenameWithPrefixAndSuffixText(state.options) || alwaysGetReferences) {
            const isDefaultExport = referenceLocation.escapedText === "default"
                || exportSpecifier.name.escapedText === "default";
            const exportKind = isDefaultExport ? ExportKind.Default : ExportKind.Named;
            const exportSymbol = Debug.checkDefined(exportSpecifier.symbol);
            const exportInfo = getExportInfo(exportSymbol, exportKind, state.checker);
            if (exportInfo) {
                searchForImportsOfExport(referenceLocation, exportSymbol, exportInfo, state);
            }
        }

        // At `export { x } from "foo"`, also search for the imported symbol `"foo".x`.
        if (search.comingFrom !== ImportExport.Export && exportDeclaration.moduleSpecifier && !propertyName && !isForRenameWithPrefixAndSuffixText(state.options)) {
            const imported = state.checker.getExportSpecifierLocalTargetSymbol(exportSpecifier);
            if (imported) searchForImportedSymbol(imported, state);
        }

        function addRef() {
            if (addReferencesHere) addReference(referenceLocation, localSymbol, state);
        }
    }

    function getLocalSymbolForExportSpecifier(referenceLocation: Identifier, referenceSymbol: Symbol, exportSpecifier: ExportSpecifier, checker: TypeChecker): Symbol {
        return isExportSpecifierAlias(referenceLocation, exportSpecifier) && checker.getExportSpecifierLocalTargetSymbol(exportSpecifier) || referenceSymbol;
    }

    /**
     * Determines if an export specifier is an alias for a reference location.
     * @param {Identifier} referenceLocation - The reference location to check against.
     * @param {ExportSpecifier} exportSpecifier - The export specifier to check.
     * @returns {boolean} - True if the export specifier is an alias for the reference location, false otherwise.
     */
    function isExportSpecifierAlias(referenceLocation: Identifier, exportSpecifier: ExportSpecifier): boolean {
        const { parent, propertyName, name } = exportSpecifier;
        Debug.assert(propertyName === referenceLocation || name === referenceLocation);
        if (propertyName) {
            // Given `export { foo as bar } [from "someModule"]`: It's an alias at `foo`, but at `bar` it's a new symbol.
            return propertyName === referenceLocation;
        }
        else {
            // `export { foo } from "foo"` is a re-export.
            // `export { foo };` is not a re-export, it creates an alias for the local variable `foo`.
            return !parent.parent.moduleSpecifier;
        }
    }

    /**
     * Searches for import or export references of a given symbol in a specified location.
     * @param {Node} referenceLocation - The location to search for references.
     * @param {Symbol} referenceSymbol - The symbol to search for references of.
     * @param {Search} search - The search options.
     * @param {State} state - The state of the search.
     * @returns {void}
     */
    function getImportOrExportReferences(referenceLocation: Node, referenceSymbol: Symbol, search: Search, state: State): void {
        const importOrExport = getImportOrExportSymbol(referenceLocation, referenceSymbol, state.checker, search.comingFrom === ImportExport.Export);
        if (!importOrExport) return;

        const { symbol } = importOrExport;

        if (importOrExport.kind === ImportExport.Import) {
            if (!(isForRenameWithPrefixAndSuffixText(state.options))) {
                searchForImportedSymbol(symbol, state);
            }
        }
        else {
            searchForImportsOfExport(referenceLocation, symbol, importOrExport.exportInfo, state);
        }
    }

    /**
     * Finds the reference for a shorthand property.
     * @param {Symbol} param0 - The symbol object containing flags and valueDeclaration.
     * @param {Search} search - The search object.
     * @param {State} state - The state object.
     * @remarks
     * Because in short-hand property assignment, an identifier which stored as name of the short-hand property assignment
     * has two meanings: property name and property value. Therefore when we do findAllReference at the position where
     * an identifier is declared, the language service should return the position of the variable declaration as well as
     * the position in short-hand property assignment excluding property accessing. However, if we do findAllReference at the
     * position of property accessing, the referenceEntry of such position will be handled in the first case.
     */
    function getReferenceForShorthandProperty({ flags, valueDeclaration }: Symbol, search: Search, state: State): void {
        const shorthandValueSymbol = state.checker.getShorthandAssignmentValueSymbol(valueDeclaration)!;
        const name = valueDeclaration && getNameOfDeclaration(valueDeclaration);
        /*
        * Because in short-hand property assignment, an identifier which stored as name of the short-hand property assignment
        * has two meanings: property name and property value. Therefore when we do findAllReference at the position where
        * an identifier is declared, the language service should return the position of the variable declaration as well as
        * the position in short-hand property assignment excluding property accessing. However, if we do findAllReference at the
        * position of property accessing, the referenceEntry of such position will be handled in the first case.
        */
        if (!(flags & SymbolFlags.Transient) && name && search.includes(shorthandValueSymbol)) {
            addReference(name, shorthandValueSymbol, state);
        }
    }

    /**
     * Adds a reference to a symbol in the given state.
     * @param {Node} referenceLocation - The location of the reference.
     * @param {Symbol | RelatedSymbol} relatedSymbol - The symbol or related symbol to add a reference to.
     * @param {State} state - The state object containing options and reference adder function.
     * @returns {void}
     */
    function addReference(referenceLocation: Node, relatedSymbol: Symbol | RelatedSymbol, state: State): void {
        const { kind, symbol } = "kind" in relatedSymbol ? relatedSymbol : { kind: undefined, symbol: relatedSymbol }; // eslint-disable-line local/no-in-operator

        // if rename symbol from default export anonymous function, for example `export default function() {}`, we do not need to add reference
        if (state.options.use === FindReferencesUse.Rename && referenceLocation.kind === SyntaxKind.DefaultKeyword) {
            return;
        }

        const addRef = state.referenceAdder(symbol);
        if (state.options.implementations) {
            addImplementationReferences(referenceLocation, addRef, state);
        }
        else {
            addRef(referenceLocation, kind);
        }
    }

    /** Adds references when a constructor is used with `new this()` in its own class and `super()` calls in subclasses.  */
    function addConstructorReferences(referenceLocation: Node, sourceFile: SourceFile, search: Search, state: State): void {
        if (isNewExpressionTarget(referenceLocation)) {
            addReference(referenceLocation, search.symbol, state);
        }

        const pusher = () => state.referenceAdder(search.symbol);

        if (isClassLike(referenceLocation.parent)) {
            Debug.assert(referenceLocation.kind === SyntaxKind.DefaultKeyword || referenceLocation.parent.name === referenceLocation);
            // This is the class declaration containing the constructor.
            findOwnConstructorReferences(search.symbol, sourceFile, pusher());
        }
        else {
            // If this class appears in `extends C`, then the extending class' "super" calls are references.
            const classExtending = tryGetClassByExtendingIdentifier(referenceLocation);
            if (classExtending) {
                findSuperConstructorAccesses(classExtending, pusher());
                findInheritedConstructorReferences(classExtending, state);
            }
        }
    }

    function addClassStaticThisReferences(referenceLocation: Node, search: Search, state: State): void {
        addReference(referenceLocation, search.symbol, state);
        const classLike = referenceLocation.parent;
        if (state.options.use === FindReferencesUse.Rename || !isClassLike(classLike)) return;
        Debug.assert(classLike.name === referenceLocation);
        const addRef = state.referenceAdder(search.symbol);
        for (const member of classLike.members) {
            if (!(isMethodOrAccessor(member) && isStatic(member))) {
                continue;
            }
            if (member.body) {
                member.body.forEachChild(function cb(node) {
                    if (node.kind === SyntaxKind.ThisKeyword) {
                        addRef(node);
                    }
                    else if (!isFunctionLike(node) && !isClassLike(node)) {
                        node.forEachChild(cb);
                    }
                });
            }
        }
    }

    /**
     * `classSymbol` is the class where the constructor was defined.
     * Reference the constructor and all calls to `new this()`.
     */
    function findOwnConstructorReferences(classSymbol: Symbol, sourceFile: SourceFile, addNode: (node: Node) => void): void {
        const constructorSymbol = getClassConstructorSymbol(classSymbol);
        if (constructorSymbol && constructorSymbol.declarations) {
            for (const decl of constructorSymbol.declarations) {
                const ctrKeyword = findChildOfKind(decl, SyntaxKind.ConstructorKeyword, sourceFile)!;
                Debug.assert(decl.kind === SyntaxKind.Constructor && !!ctrKeyword);
                addNode(ctrKeyword);
            }
        }

        if (classSymbol.exports) {
            classSymbol.exports.forEach(member => {
                const decl = member.valueDeclaration;
                if (decl && decl.kind === SyntaxKind.MethodDeclaration) {
                    const body = (decl as MethodDeclaration).body;
                    if (body) {
                        forEachDescendantOfKind(body, SyntaxKind.ThisKeyword, thisKeyword => {
                            if (isNewExpressionTarget(thisKeyword)) {
                                addNode(thisKeyword);
                            }
                        });
                    }
                }
            });
        }
    }

    function getClassConstructorSymbol(classSymbol: Symbol): Symbol | undefined {
        return classSymbol.members && classSymbol.members.get(InternalSymbolName.Constructor);
    }

    /** Find references to `super` in the constructor of an extending class.  */
    function findSuperConstructorAccesses(classDeclaration: ClassLikeDeclaration, addNode: (node: Node) => void): void {
        const constructor = getClassConstructorSymbol(classDeclaration.symbol);
        if (!(constructor && constructor.declarations)) {
            return;
        }

        for (const decl of constructor.declarations) {
            Debug.assert(decl.kind === SyntaxKind.Constructor);
            const body = (decl as ConstructorDeclaration).body;
            if (body) {
                forEachDescendantOfKind(body, SyntaxKind.SuperKeyword, node => {
                    if (isCallExpressionTarget(node)) {
                        addNode(node);
                    }
                });
            }
        }
    }

    function hasOwnConstructor(classDeclaration: ClassLikeDeclaration): boolean {
        return !!getClassConstructorSymbol(classDeclaration.symbol);
    }

    function findInheritedConstructorReferences(classDeclaration: ClassLikeDeclaration, state: State): void {
        if (hasOwnConstructor(classDeclaration)) return;
        const classSymbol = classDeclaration.symbol;
        const search = state.createSearch(/*location*/ undefined, classSymbol, /*comingFrom*/ undefined);
        getReferencesInContainerOrFiles(classSymbol, state, search);
    }

    function addImplementationReferences(refNode: Node, addReference: (node: Node) => void, state: State): void {
        // Check if we found a function/propertyAssignment/method with an implementation or initializer
        if (isDeclarationName(refNode) && isImplementation(refNode.parent)) {
            addReference(refNode);
            return;
        }

        if (refNode.kind !== SyntaxKind.Identifier) {
            return;
        }

        if (refNode.parent.kind === SyntaxKind.ShorthandPropertyAssignment) {
            // Go ahead and dereference the shorthand assignment by going to its definition
            getReferenceEntriesForShorthandPropertyAssignment(refNode, state.checker, addReference);
        }

        // Check if the node is within an extends or implements clause
        const containingNode = getContainingNodeIfInHeritageClause(refNode);
        if (containingNode) {
            addReference(containingNode);
            return;
        }

        // If we got a type reference, try and see if the reference applies to any expressions that can implement an interface
        // Find the first node whose parent isn't a type node -- i.e., the highest type node.
        const typeNode = findAncestor(refNode, a => !isQualifiedName(a.parent) && !isTypeNode(a.parent) && !isTypeElement(a.parent))!;
        const typeHavingNode = typeNode.parent;
        if (hasType(typeHavingNode) && typeHavingNode.type === typeNode && state.markSeenContainingTypeReference(typeHavingNode)) {
            if (hasInitializer(typeHavingNode)) {
                addIfImplementation(typeHavingNode.initializer!);
            }
            else if (isFunctionLike(typeHavingNode) && (typeHavingNode as FunctionLikeDeclaration).body) {
                const body = (typeHavingNode as FunctionLikeDeclaration).body!;
                if (body.kind === SyntaxKind.Block) {
                    forEachReturnStatement(body as Block, returnStatement => {
                        if (returnStatement.expression) addIfImplementation(returnStatement.expression);
                    });
                }
                else {
                    addIfImplementation(body);
                }
            }
            else if (isAssertionExpression(typeHavingNode)) {
                addIfImplementation(typeHavingNode.expression);
            }
        }

        function addIfImplementation(e: Expression): void {
            if (isImplementationExpression(e)) addReference(e);
        }
    }

    function getContainingNodeIfInHeritageClause(node: Node): ClassLikeDeclaration | InterfaceDeclaration | undefined {
        return isIdentifier(node) || isPropertyAccessExpression(node) ? getContainingNodeIfInHeritageClause(node.parent)
            : isExpressionWithTypeArguments(node) ? tryCast(node.parent.parent, or(isClassLike, isInterfaceDeclaration)) : undefined;
    }

    /**
     * Determines if the provided expression can be considered an implementation.
     * @param {Expression} node - The expression to check.
     * @returns {boolean} - True if the expression is an implementation, false otherwise.
     */
    function isImplementationExpression(node: Expression): boolean {
        switch (node.kind) {
            case SyntaxKind.ParenthesizedExpression:
                return isImplementationExpression((node as ParenthesizedExpression).expression);
            case SyntaxKind.ArrowFunction:
            case SyntaxKind.FunctionExpression:
            case SyntaxKind.ObjectLiteralExpression:
            case SyntaxKind.ClassExpression:
            case SyntaxKind.ArrayLiteralExpression:
                return true;
            default:
                return false;
        }
    }

    /**
     * Determines if the parent symbol occurs somewhere in the child's ancestry. If the parent symbol
     * is an interface, determines if some ancestor of the child symbol extends or inherits from it.
     * Also takes in a cache of previous results which makes this slightly more efficient and is
     * necessary to avoid potential loops like so:
     * class A extends B { }
     * class B extends A { }
     *
     * We traverse the AST rather than using the type checker because users are typically only interested
     * in explicit implementations of an interface/class when calling "Go to Implementation". Sibling
     * implementations of types that share a common ancestor with the type whose implementation we are
     * searching for need to be filtered out of the results. The type checker doesn't let us make the
     * distinction between structurally compatible implementations and explicit implementations, so we
     * must use the AST.
     *
     * @param symbol         A class or interface Symbol
     * @param parent        Another class or interface Symbol
     * @param cachedResults A map of symbol id pairs (i.e. "child,parent") to booleans indicating previous results
     * @param checker       A TypeChecker instance
     * @returns              A boolean indicating if the parent symbol occurs somewhere in the child's ancestry
     */
    function explicitlyInheritsFrom(symbol: Symbol, parent: Symbol, cachedResults: Map<string, boolean>, checker: TypeChecker): boolean {
        if (symbol === parent) {
            return true;
        }

        const key = getSymbolId(symbol) + "," + getSymbolId(parent);
        const cached = cachedResults.get(key);
        if (cached !== undefined) {
            return cached;
        }

        // Set the key so that we don't infinitely recurse
        cachedResults.set(key, false);

        const inherits = !!symbol.declarations && symbol.declarations.some(declaration =>
            getAllSuperTypeNodes(declaration).some(typeReference => {
                const type = checker.getTypeAtLocation(typeReference);
                return !!type && !!type.symbol && explicitlyInheritsFrom(type.symbol, parent, cachedResults, checker);
            }));
        cachedResults.set(key, inherits);
        return inherits;
    }

    /**
     * Returns an array of symbol references for the 'super' keyword within a given node.
     * @param {Node} superKeyword - The 'super' keyword node to search for references.
     * @returns {SymbolAndEntries[] | undefined} - An array of symbol references for the 'super' keyword, or undefined if none are found.
     */
    function getReferencesForSuperKeyword(superKeyword: Node): SymbolAndEntries[] | undefined {
        let searchSpaceNode: SuperContainer | ClassLikeDeclaration | TypeLiteralNode | InterfaceDeclaration | ObjectLiteralExpression | undefined = getSuperContainer(superKeyword, /*stopOnFunctions*/ false);
        if (!searchSpaceNode) {
            return undefined;
        }
        // Whether 'super' occurs in a static context within a class.
        let staticFlag = ModifierFlags.Static;

        switch (searchSpaceNode.kind) {
            case SyntaxKind.PropertyDeclaration:
            case SyntaxKind.PropertySignature:
            case SyntaxKind.MethodDeclaration:
            case SyntaxKind.MethodSignature:
            case SyntaxKind.Constructor:
            case SyntaxKind.GetAccessor:
            case SyntaxKind.SetAccessor:
                staticFlag &= getSyntacticModifierFlags(searchSpaceNode);
                searchSpaceNode = searchSpaceNode.parent; // re-assign to be the owning class
                break;
            default:
                return undefined;
        }

        const sourceFile = searchSpaceNode.getSourceFile();
        const references = mapDefined(getPossibleSymbolReferenceNodes(sourceFile, "super", searchSpaceNode), node => {
            if (node.kind !== SyntaxKind.SuperKeyword) {
                return;
            }

            const container = getSuperContainer(node, /*stopOnFunctions*/ false);

            // If we have a 'super' container, we must have an enclosing class.
            // Now make sure the owning class is the same as the search-space
            // and has the same static qualifier as the original 'super's owner.
            return container && isStatic(container) === !!staticFlag && container.parent.symbol === searchSpaceNode!.symbol ? nodeEntry(node) : undefined;
        });

        return [{ definition: { type: DefinitionKind.Symbol, symbol: searchSpaceNode.symbol }, references }];
    }

    function isParameterName(node: Node) {
        return node.kind === SyntaxKind.Identifier && node.parent.kind === SyntaxKind.Parameter && (node.parent as ParameterDeclaration).name === node;
    }

    /**
     * Returns an array of SymbolAndEntries objects representing references to the 'this' keyword within the given search space node. The search space node is determined by the given thisOrSuperKeyword parameter and its container hierarchy. If no references are found, returns undefined.
     * @param {Node} thisOrSuperKeyword - The 'this' or 'super' keyword node to search for references to.
     * @param {readonly SourceFile[]} sourceFiles - An array of source files to search within if the search space node is a SourceFile.
     * @param {CancellationToken} cancellationToken - A token that can be used to cancel the search operation.
     * @returns {SymbolAndEntries[] | undefined} An array of SymbolAndEntries objects representing references to the 'this' keyword within the given search space node, or undefined if no references are found.
     */
    function getReferencesForThisKeyword(thisOrSuperKeyword: Node, sourceFiles: readonly SourceFile[], cancellationToken: CancellationToken): SymbolAndEntries[] | undefined {
        let searchSpaceNode: Node = getThisContainer(thisOrSuperKeyword, /*includeArrowFunctions*/ false, /*includeClassComputedPropertyName*/ false);

        // Whether 'this' occurs in a static context within a class.
        let staticFlag = ModifierFlags.Static;

        switch (searchSpaceNode.kind) {
            case SyntaxKind.MethodDeclaration:
            case SyntaxKind.MethodSignature:
                if (isObjectLiteralMethod(searchSpaceNode)) {
                    staticFlag &= getSyntacticModifierFlags(searchSpaceNode);
                    searchSpaceNode = searchSpaceNode.parent; // re-assign to be the owning object literals
                    break;
                }
                // falls through
            case SyntaxKind.PropertyDeclaration:
            case SyntaxKind.PropertySignature:
            case SyntaxKind.Constructor:
            case SyntaxKind.GetAccessor:
            case SyntaxKind.SetAccessor:
                staticFlag &= getSyntacticModifierFlags(searchSpaceNode);
                searchSpaceNode = searchSpaceNode.parent; // re-assign to be the owning class
                break;
            case SyntaxKind.SourceFile:
                if (isExternalModule(searchSpaceNode as SourceFile) || isParameterName(thisOrSuperKeyword)) {
                    return undefined;
                }
                // falls through
            case SyntaxKind.FunctionDeclaration:
            case SyntaxKind.FunctionExpression:
                break;
            // Computed properties in classes are not handled here because references to this are illegal,
            // so there is no point finding references to them.
            default:
                return undefined;
        }

        const references = flatMap(searchSpaceNode.kind === SyntaxKind.SourceFile ? sourceFiles : [searchSpaceNode.getSourceFile()], sourceFile => {
            cancellationToken.throwIfCancellationRequested();
            return getPossibleSymbolReferenceNodes(sourceFile, "this", isSourceFile(searchSpaceNode) ? sourceFile : searchSpaceNode).filter(node => {
                if (!isThis(node)) {
                    return false;
                }
                const container = getThisContainer(node, /*includeArrowFunctions*/ false, /*includeClassComputedPropertyName*/ false);
                if (!canHaveSymbol(container)) return false;
                switch (searchSpaceNode.kind) {
                    case SyntaxKind.FunctionExpression:
                    case SyntaxKind.FunctionDeclaration:
                        return (searchSpaceNode as FunctionExpression | FunctionDeclaration).symbol === container.symbol;
                    case SyntaxKind.MethodDeclaration:
                    case SyntaxKind.MethodSignature:
                        return isObjectLiteralMethod(searchSpaceNode) && searchSpaceNode.symbol === container.symbol;
                    case SyntaxKind.ClassExpression:
                    case SyntaxKind.ClassDeclaration:
                    case SyntaxKind.ObjectLiteralExpression:
                        // Make sure the container belongs to the same class/object literals
                        // and has the appropriate static modifier from the original container.
                        return container.parent && canHaveSymbol(container.parent) && (searchSpaceNode as ClassLikeDeclaration | ObjectLiteralExpression).symbol === container.parent.symbol && isStatic(container) === !!staticFlag;
                    case SyntaxKind.SourceFile:
                        return container.kind === SyntaxKind.SourceFile && !isExternalModule(container) && !isParameterName(node);
                }
            });
        }).map(n => nodeEntry(n));

        const thisParameter = firstDefined(references, r => isParameter(r.node.parent) ? r.node : undefined);
        return [{
            definition: { type: DefinitionKind.This, node: thisParameter || thisOrSuperKeyword },
            references
        }];
    }

    /**
     * Returns an array of SymbolAndEntries objects containing information about references to a given StringLiteralLike node in an array of source files.
     * @param {StringLiteralLike} node - The node to search for references.
     * @param {readonly SourceFile[]} sourceFiles - The array of source files to search in.
     * @param {TypeChecker} checker - The TypeChecker object to use for type checking.
     * @param {CancellationToken} cancellationToken - The CancellationToken object to use for cancellation.
     * @returns {SymbolAndEntries[]} An array of SymbolAndEntries objects containing information about references to the given node.
     */
    function getReferencesForStringLiteral(node: StringLiteralLike, sourceFiles: readonly SourceFile[], checker: TypeChecker, cancellationToken: CancellationToken): SymbolAndEntries[] {
        const type = getContextualTypeFromParentOrAncestorTypeNode(node, checker);
        const references = flatMap(sourceFiles, sourceFile => {
            cancellationToken.throwIfCancellationRequested();
            return mapDefined(getPossibleSymbolReferenceNodes(sourceFile, node.text), ref => {
                if (isStringLiteralLike(ref) && ref.text === node.text) {
                    if (type) {
                        const refType = getContextualTypeFromParentOrAncestorTypeNode(ref, checker);
                        if (type !== checker.getStringType() && type === refType) {
                            return nodeEntry(ref, EntryKind.StringLiteral);
                        }
                    }
                    else {
                        return isNoSubstitutionTemplateLiteral(ref) && !rangeIsOnSingleLine(ref, sourceFile) ? undefined :
                            nodeEntry(ref, EntryKind.StringLiteral);
                    }
                }
            });
        });

        return [{
            definition: { type: DefinitionKind.String, node },
            references
        }];
    }

    // For certain symbol kinds, we need to include other symbols in the search set.
    // This is not needed when searching for re-exports.
    /**
     * Returns an array of symbols related to the given symbol and location.
     * @param symbol - The symbol to search for related symbols.
     * @param location - The location to search for related symbols.
     * @param checker - The type checker to use for symbol analysis.
     * @param isForRename - A boolean indicating whether the search is for renaming.
     * @param providePrefixAndSuffixText - A boolean indicating whether to provide prefix and suffix text.
     * @param implementations - A boolean indicating whether to search for implementations.
     * @returns An array of symbols related to the given symbol and location.
     */
    function populateSearchSymbolSet(symbol: Symbol, location: Node, checker: TypeChecker, isForRename: boolean, providePrefixAndSuffixText: boolean, implementations: boolean): Symbol[] {
        const result: Symbol[] = [];
        forEachRelatedSymbol<void>(symbol, location, checker, isForRename, !(isForRename && providePrefixAndSuffixText),
            (sym, root, base) => {
                // static method/property and instance method/property might have the same name. Only include static or only include instance.
                if (base) {
                    if (isStaticSymbol(symbol) !== isStaticSymbol(base)) {
                        base = undefined;
                    }
                }
                result.push(base || root || sym);
            },
            // when try to find implementation, implementations is true, and not allowed to find base class
            /*allowBaseTypes*/() => !implementations);
        return result;
    }

    /**
     * Finds a related symbol to the given symbol at a specific location.
     * @template T The type of the related symbol.
     * @param symbol The symbol to find a related symbol for.
     * @param location The location to search for the related symbol.
     * @param checker The TypeChecker instance to use.
     * @param isForRenamePopulateSearchSymbolSet Whether the search is for renaming or populating the search symbol set.
     * @param onlyIncludeBindingElementAtReferenceLocation Whether to only include binding elements at the reference location.
     * @param cbSymbol A callback function to handle the found related symbol.
     * @param allowBaseTypes A function that returns true if it should try to find in base class or interface.
     * @returns The related symbol if found, undefined otherwise.
     */
    function forEachRelatedSymbol<T>(
        symbol: Symbol, location: Node, checker: TypeChecker, isForRenamePopulateSearchSymbolSet: boolean, onlyIncludeBindingElementAtReferenceLocation: boolean,
        /**
         * @param baseSymbol This symbol means one property/mehtod from base class or interface when it is not null or undefined,
         */
        cbSymbol: (symbol: Symbol, rootSymbol?: Symbol, baseSymbol?: Symbol, kind?: NodeEntryKind) => T | undefined,
        allowBaseTypes: (rootSymbol: Symbol) => boolean,
    ): T | undefined {
        const containingObjectLiteralElement = getContainingObjectLiteralElement(location);
        if (containingObjectLiteralElement) {
            /* Because in short-hand property assignment, location has two meaning : property name and as value of the property
            * When we do findAllReference at the position of the short-hand property assignment, we would want to have references to position of
            * property name and variable declaration of the identifier.
            * Like in below example, when querying for all references for an identifier 'name', of the property assignment, the language service
            * should show both 'name' in 'obj' and 'name' in variable declaration
            *      const name = "Foo";
            *      const obj = { name };
            * In order to do that, we will populate the search set with the value symbol of the identifier as a value of the property assignment
            * so that when matching with potential reference symbol, both symbols from property declaration and variable declaration
            * will be included correctly.
            */
            const shorthandValueSymbol = checker.getShorthandAssignmentValueSymbol(location.parent); // gets the local symbol
            if (shorthandValueSymbol && isForRenamePopulateSearchSymbolSet) {
                // When renaming 'x' in `const o = { x }`, just rename the local variable, not the property.
                return cbSymbol(shorthandValueSymbol, /*rootSymbol*/ undefined, /*baseSymbol*/ undefined, EntryKind.SearchedLocalFoundProperty);
            }

            // If the location is in a context sensitive location (i.e. in an object literal) try
            // to get a contextual type for it, and add the property symbol from the contextual
            // type to the search set
            const contextualType = checker.getContextualType(containingObjectLiteralElement.parent);
            const res = contextualType && firstDefined(
                getPropertySymbolsFromContextualType(containingObjectLiteralElement, checker, contextualType, /*unionSymbolOk*/ true),
                sym => fromRoot(sym, EntryKind.SearchedPropertyFoundLocal));
            if (res) return res;

            // If the location is name of property symbol from object literal destructuring pattern
            // Search the property symbol
            //      for ( { property: p2 } of elems) { }
            const propertySymbol = getPropertySymbolOfDestructuringAssignment(location, checker);
            const res1 = propertySymbol && cbSymbol(propertySymbol, /*rootSymbol*/ undefined, /*baseSymbol*/ undefined, EntryKind.SearchedPropertyFoundLocal);
            if (res1) return res1;

            const res2 = shorthandValueSymbol && cbSymbol(shorthandValueSymbol, /*rootSymbol*/ undefined, /*baseSymbol*/ undefined, EntryKind.SearchedLocalFoundProperty);
            if (res2) return res2;
        }

        const aliasedSymbol = getMergedAliasedSymbolOfNamespaceExportDeclaration(location, symbol, checker);
        if (aliasedSymbol) {
            // In case of UMD module and global merging, search for global as well
            const res = cbSymbol(aliasedSymbol, /*rootSymbol*/ undefined, /*baseSymbol*/ undefined, EntryKind.Node);
            if (res) return res;
        }

        const res = fromRoot(symbol);
        if (res) return res;

        if (symbol.valueDeclaration && isParameterPropertyDeclaration(symbol.valueDeclaration, symbol.valueDeclaration.parent)) {
            // For a parameter property, now try on the other symbol (property if this was a parameter, parameter if this was a property).
            const paramProps = checker.getSymbolsOfParameterPropertyDeclaration(cast(symbol.valueDeclaration, isParameter), symbol.name);
            Debug.assert(paramProps.length === 2 && !!(paramProps[0].flags & SymbolFlags.FunctionScopedVariable) && !!(paramProps[1].flags & SymbolFlags.Property)); // is [parameter, property]
            return fromRoot(symbol.flags & SymbolFlags.FunctionScopedVariable ? paramProps[1] : paramProps[0]);
        }

        const exportSpecifier = getDeclarationOfKind<ExportSpecifier>(symbol, SyntaxKind.ExportSpecifier);
        if (!isForRenamePopulateSearchSymbolSet || exportSpecifier && !exportSpecifier.propertyName) {
            const localSymbol = exportSpecifier && checker.getExportSpecifierLocalTargetSymbol(exportSpecifier);
            if (localSymbol) {
                const res = cbSymbol(localSymbol, /*rootSymbol*/ undefined, /*baseSymbol*/ undefined, EntryKind.Node);
                if (res) return res;
            }
        }

        // symbolAtLocation for a binding element is the local symbol. See if the search symbol is the property.
        // Don't do this when populating search set for a rename when prefix and suffix text will be provided -- just rename the local.
        if (!isForRenamePopulateSearchSymbolSet) {
            let bindingElementPropertySymbol: Symbol | undefined;
            if (onlyIncludeBindingElementAtReferenceLocation) {
                bindingElementPropertySymbol = isObjectBindingElementWithoutPropertyName(location.parent) ? getPropertySymbolFromBindingElement(checker, location.parent) : undefined;
            }
            else {
                bindingElementPropertySymbol = getPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol, checker);
            }
            return bindingElementPropertySymbol && fromRoot(bindingElementPropertySymbol, EntryKind.SearchedPropertyFoundLocal);
        }

        Debug.assert(isForRenamePopulateSearchSymbolSet);
        // due to the above assert and the arguments at the uses of this function,
        // (onlyIncludeBindingElementAtReferenceLocation <=> !providePrefixAndSuffixTextForRename) holds
        const includeOriginalSymbolOfBindingElement = onlyIncludeBindingElementAtReferenceLocation;

        if (includeOriginalSymbolOfBindingElement) {
            const bindingElementPropertySymbol = getPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol, checker);
            return bindingElementPropertySymbol && fromRoot(bindingElementPropertySymbol, EntryKind.SearchedPropertyFoundLocal);
        }

        /**
         * Returns the root symbol of a given symbol, along with any related symbols.
         * @param sym - The symbol to find the root symbol for.
         * @param kind - Optional parameter to specify the kind of node entry.
         * @returns The root symbol, or undefined if none is found.
         */
        function fromRoot(sym: Symbol, kind?: NodeEntryKind): T | undefined {
            // If this is a union property:
            //   - In populateSearchSymbolsSet we will add all the symbols from all its source symbols in all unioned types.
            //   - In findRelatedSymbol, we will just use the union symbol if any source symbol is included in the search.
            // If the symbol is an instantiation from a another symbol (e.g. widened symbol):
            //   - In populateSearchSymbolsSet, add the root the list
            //   - In findRelatedSymbol, return the source symbol if that is in the search. (Do not return the instantiation symbol.)
            return firstDefined(checker.getRootSymbols(sym), rootSymbol =>
                cbSymbol(sym, rootSymbol, /*baseSymbol*/ undefined, kind)
                // Add symbol of properties/methods of the same name in base classes and implemented interfaces definitions
                || (rootSymbol.parent && rootSymbol.parent.flags & (SymbolFlags.Class | SymbolFlags.Interface) && allowBaseTypes(rootSymbol)
                    ? getPropertySymbolsFromBaseTypes(rootSymbol.parent, rootSymbol.name, checker, base => cbSymbol(sym, rootSymbol, base, kind))
                    : undefined));
        }

        function getPropertySymbolOfObjectBindingPatternWithoutPropertyName(symbol: Symbol, checker: TypeChecker): Symbol | undefined {
            const bindingElement = getDeclarationOfKind<BindingElement>(symbol, SyntaxKind.BindingElement);
            if (bindingElement && isObjectBindingElementWithoutPropertyName(bindingElement)) {
                return getPropertySymbolFromBindingElement(checker, bindingElement);
            }
        }
    }

    /**
     * Retrieves the symbol of the given property-name from the base types of the provided symbol.
     * @template T - The type of the symbol to be returned.
     * @param symbol - The symbol to start searching for the given propertyName.
     * @param propertyName - The name of the property to search for.
     * @param checker - The TypeChecker instance to use for type checking.
     * @param cb - A callback function to be called on the found symbol.
     * @returns The symbol of the found property symbols or undefined if not found.
     */
    function getPropertySymbolsFromBaseTypes<T>(symbol: Symbol, propertyName: string, checker: TypeChecker, cb: (symbol: Symbol) => T | undefined): T | undefined {
        const seen = new Map<SymbolId, true>();
        return recur(symbol);

        /**
         * Recursively searches for a property symbol in a given symbol's declarations and its super types.
         * @param symbol - The symbol to search for the property symbol.
         * @returns The property symbol or undefined if not found.
         */
        function recur(symbol: Symbol): T | undefined {
            // Use `addToSeen` to ensure we don't infinitely recurse in this situation:
            //      interface C extends C {
            //          /*findRef*/propName: string;
            //      }
            if (!(symbol.flags & (SymbolFlags.Class | SymbolFlags.Interface)) || !addToSeen(seen, getSymbolId(symbol))) return;

            return firstDefined(symbol.declarations, declaration => firstDefined(getAllSuperTypeNodes(declaration), typeReference => {
                const type = checker.getTypeAtLocation(typeReference);
                const propertySymbol = type && type.symbol && checker.getPropertyOfType(type, propertyName);
                // Visit the typeReference as well to see if it directly or indirectly uses that property
                return type && propertySymbol && (firstDefined(checker.getRootSymbols(propertySymbol), cb) || recur(type.symbol));
            }));
        }
    }

    interface RelatedSymbol {
        readonly symbol: Symbol;
        readonly kind: NodeEntryKind | undefined;
    }

    function isStaticSymbol(symbol: Symbol): boolean {
        if (!symbol.valueDeclaration) return false;
        const modifierFlags = getEffectiveModifierFlags(symbol.valueDeclaration);
        return !!(modifierFlags & ModifierFlags.Static);
    }

    /**
     * Returns a RelatedSymbol object if the referenceSymbol is related to the search criteria.
     * @param {Search} search - The search criteria.
     * @param {Symbol} referenceSymbol - The reference symbol to compare against.
     * @param {Node} referenceLocation - The reference location.
     * @param {State} state - The state object.
     * @returns {RelatedSymbol | undefined} - The RelatedSymbol object if found, otherwise undefined.
     */
    function getRelatedSymbol(search: Search, referenceSymbol: Symbol, referenceLocation: Node, state: State): RelatedSymbol | undefined {
        const { checker } = state;
        return forEachRelatedSymbol(referenceSymbol, referenceLocation, checker, /*isForRenamePopulateSearchSymbolSet*/ false,
            /*onlyIncludeBindingElementAtReferenceLocation*/ state.options.use !== FindReferencesUse.Rename || !!state.options.providePrefixAndSuffixTextForRename,
            (sym, rootSymbol, baseSymbol, kind): RelatedSymbol | undefined => {
                // check whether the symbol used to search itself is just the searched one.
                if (baseSymbol) {
                    // static method/property and instance method/property might have the same name. Only check static or only check instance.
                    if (isStaticSymbol(referenceSymbol) !== isStaticSymbol(baseSymbol)) {
                        baseSymbol = undefined;
                    }
                }
                return search.includes(baseSymbol || rootSymbol || sym)
                    // For a base type, use the symbol for the derived type. For a synthetic (e.g. union) property, use the union symbol.
                    ? { symbol: rootSymbol && !(getCheckFlags(sym) & CheckFlags.Synthetic) ? rootSymbol : sym, kind }
                    : undefined;
            },
            /*allowBaseTypes*/ rootSymbol =>
                !(search.parents && !search.parents.some(parent => explicitlyInheritsFrom(rootSymbol.parent!, parent, state.inheritsFromCache, checker)))
        );
    }

    /**
     * Given a node and a symbol, this function determines the semantic meaning of the node by iterating through the declarations of the symbol and checking for intersection with the initial meaning of the node. The result is order-sensitive, meaning that if the initial meaning intersects with multiple declarations, all of them will be considered.
     * @param {Node} node - The node to determine the meaning of.
     * @param {Symbol} symbol - The symbol to check declarations of.
     * @returns {SemanticMeaning} - The semantic meaning of the node.
     */
    export function getIntersectingMeaningFromDeclarations(node: Node, symbol: Symbol): SemanticMeaning {
        let meaning = getMeaningFromLocation(node);
        const { declarations } = symbol;
        if (declarations) {
            let lastIterationMeaning: SemanticMeaning;
            do {
                // The result is order-sensitive, for instance if initialMeaning === Namespace, and declarations = [class, instantiated module]
                // we need to consider both as they initialMeaning intersects with the module in the namespace space, and the module
                // intersects with the class in the value space.
                // To achieve that we will keep iterating until the result stabilizes.

                // Remember the last meaning
                lastIterationMeaning = meaning;

                for (const declaration of declarations) {
                    const declarationMeaning = getMeaningFromDeclaration(declaration);

                    if (declarationMeaning & meaning) {
                        meaning |= declarationMeaning;
                    }
                }
            }
            while (meaning !== lastIterationMeaning);
        }
        return meaning;
    }

    function isImplementation(node: Node): boolean {
        return !!(node.flags & NodeFlags.Ambient) ? !(isInterfaceDeclaration(node) || isTypeAliasDeclaration(node)) :
            (isVariableLike(node) ? hasInitializer(node) :
            isFunctionLikeDeclaration(node) ? !!node.body :
            isClassLike(node) || isModuleOrEnumDeclaration(node));
    }

    /**
     * Retrieves reference entries for shorthand property assignments.
     * @param node - The node to retrieve reference entries for.
     * @param checker - The type checker to use.
     * @param addReference - The function to add reference entries to.
     */
    export function getReferenceEntriesForShorthandPropertyAssignment(node: Node, checker: TypeChecker, addReference: (node: Node) => void): void {
        const refSymbol = checker.getSymbolAtLocation(node)!;
        const shorthandSymbol = checker.getShorthandAssignmentValueSymbol(refSymbol.valueDeclaration);

        if (shorthandSymbol) {
            for (const declaration of shorthandSymbol.getDeclarations()!) {
                if (getMeaningFromDeclaration(declaration) & SemanticMeaning.Value) {
                    addReference(declaration);
                }
            }
        }
    }

    function forEachDescendantOfKind(node: Node, kind: SyntaxKind, action: (node: Node) => void): void {
        forEachChild(node, child => {
            if (child.kind === kind) {
                action(child);
            }
            forEachDescendantOfKind(child, kind, action);
        });
    }

    /** Get `C` given `N` if `N` is in the position `class C extends N` or `class C extends foo.N` where `N` is an identifier. */
    function tryGetClassByExtendingIdentifier(node: Node): ClassLikeDeclaration | undefined {
        return tryGetClassExtendingExpressionWithTypeArguments(climbPastPropertyAccess(node).parent);
    }

    /**
     * If we are just looking for implementations and this is a property access expression, we need to get the
     * symbol of the local type of the symbol the property is being accessed on. This is because our search
     * symbol may have a different parent symbol if the local type's symbol does not declare the property
     * being accessed (i.e. it is declared in some parent class or interface)
     */
    function getParentSymbolsOfPropertyAccess(location: Node, symbol: Symbol, checker: TypeChecker): readonly Symbol[] | undefined {
        const propertyAccessExpression = isRightSideOfPropertyAccess(location) ? location.parent as PropertyAccessExpression : undefined;
        const lhsType = propertyAccessExpression && checker.getTypeAtLocation(propertyAccessExpression.expression);
        const res = mapDefined(lhsType && (lhsType.isUnionOrIntersection() ? lhsType.types : lhsType.symbol === symbol.parent ? undefined : [lhsType]), t =>
            t.symbol && t.symbol.flags & (SymbolFlags.Class | SymbolFlags.Interface) ? t.symbol : undefined);
        return res.length === 0 ? undefined : res;
    }

    function isForRenameWithPrefixAndSuffixText(options: Options) {
        return options.use === FindReferencesUse.Rename && options.providePrefixAndSuffixTextForRename;
    }
}
