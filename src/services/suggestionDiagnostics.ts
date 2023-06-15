import {
    addRange,
    AnyValidImportOrReExport,
    ArrowFunction,
    AssignmentDeclarationKind,
    Block,
    CallExpression,
    CancellationToken,
    codefix,
    compilerOptionsIndicateEsModules,
    createDiagnosticForNode,
    Diagnostics,
    DiagnosticWithLocation,
    Expression,
    ExpressionStatement,
    Extension,
    fileExtensionIsOneOf,
    forEachReturnStatement,
    FunctionDeclaration,
    FunctionExpression,
    FunctionFlags,
    FunctionLikeDeclaration,
    getAllowSyntheticDefaultImports,
    getAssignmentDeclarationKind,
    getFunctionFlags,
    getModeForUsageLocation,
    getResolvedModule,
    hasInitializer,
    hasPropertyAccessExpressionWithName,
    Identifier,
    importFromModuleSpecifier,
    isAsyncFunction,
    isBinaryExpression,
    isBlock,
    isCallExpression,
    isExportAssignment,
    isFunctionDeclaration,
    isFunctionExpression,
    isFunctionLike,
    isIdentifier,
    isPropertyAccessExpression,
    isRequireCall,
    isReturnStatement,
    isSourceFileJS,
    isStringLiteral,
    isVariableDeclaration,
    isVariableStatement,
    MethodDeclaration,
    ModuleKind,
    Node,
    NodeFlags,
    Program,
    programContainsEsModules,
    PropertyAccessExpression,
    ReturnStatement,
    skipAlias,
    some,
    SourceFile,
    SyntaxKind,
    TypeChecker,
    VariableStatement,
} from "./_namespaces/ts";

const visitedNestedConvertibleFunctions = new Map<string, true>();

/**
 * Computes suggestion diagnostics for a given source file and program.
 * @param sourceFile - The source file to compute suggestion diagnostics for.
 * @param program - The program to use for type checking.
 * @param cancellationToken - Optional cancellation token.
 * @returns An array of DiagnosticWithLocation objects representing the suggestion diagnostics.
 */
export function computeSuggestionDiagnostics(sourceFile: SourceFile, program: Program, cancellationToken: CancellationToken): DiagnosticWithLocation[] {
    program.getSemanticDiagnostics(sourceFile, cancellationToken);
    const diags: DiagnosticWithLocation[] = [];
    const checker = program.getTypeChecker();
    const isCommonJSFile = sourceFile.impliedNodeFormat === ModuleKind.CommonJS || fileExtensionIsOneOf(sourceFile.fileName, [Extension.Cts, Extension.Cjs]) ;

    if (!isCommonJSFile &&
        sourceFile.commonJsModuleIndicator &&
        (programContainsEsModules(program) || compilerOptionsIndicateEsModules(program.getCompilerOptions())) &&
        containsTopLevelCommonjs(sourceFile)) {
        diags.push(createDiagnosticForNode(getErrorNodeFromCommonJsIndicator(sourceFile.commonJsModuleIndicator), Diagnostics.File_is_a_CommonJS_module_it_may_be_converted_to_an_ES_module));
    }

    const isJsFile = isSourceFileJS(sourceFile);

    visitedNestedConvertibleFunctions.clear();
    check(sourceFile);

    if (getAllowSyntheticDefaultImports(program.getCompilerOptions())) {
        for (const moduleSpecifier of sourceFile.imports) {
            const importNode = importFromModuleSpecifier(moduleSpecifier);
            const name = importNameForConvertToDefaultImport(importNode);
            if (!name) continue;
            const module = getResolvedModule(sourceFile, moduleSpecifier.text, getModeForUsageLocation(sourceFile, moduleSpecifier));
            const resolvedFile = module && program.getSourceFile(module.resolvedFileName);
            if (resolvedFile && resolvedFile.externalModuleIndicator && resolvedFile.externalModuleIndicator !== true && isExportAssignment(resolvedFile.externalModuleIndicator) && resolvedFile.externalModuleIndicator.isExportEquals) {
                diags.push(createDiagnosticForNode(name, Diagnostics.Import_may_be_converted_to_a_default_import));
            }
        }
    }

    addRange(diags, sourceFile.bindSuggestionDiagnostics);
    addRange(diags, program.getSuggestionDiagnostics(sourceFile, cancellationToken));
    return diags.sort((d1, d2) => d1.start - d2.start);

    /**
     * Checks a Node for potential code fixes and adds corresponding diagnostics to the provided array.
     * @param node The Node to check.
     * @returns void
     */
    function check(node: Node) {
        if (isJsFile) {
            if (canBeConvertedToClass(node, checker)) {
                diags.push(createDiagnosticForNode(isVariableDeclaration(node.parent) ? node.parent.name : node, Diagnostics.This_constructor_function_may_be_converted_to_a_class_declaration));
            }
        }
        else {
            if (isVariableStatement(node) &&
                node.parent === sourceFile &&
                node.declarationList.flags & NodeFlags.Const &&
                node.declarationList.declarations.length === 1) {
                const init = node.declarationList.declarations[0].initializer;
                if (init && isRequireCall(init, /*requireStringLiteralLikeArgument*/ true)) {
                    diags.push(createDiagnosticForNode(init, Diagnostics.require_call_may_be_converted_to_an_import));
                }
            }

            const jsdocTypedefNode = codefix.getJSDocTypedefNode(node);
            if (jsdocTypedefNode) {
                diags.push(createDiagnosticForNode(jsdocTypedefNode, Diagnostics.JSDoc_typedef_may_be_converted_to_TypeScript_type));
            }

            if (codefix.parameterShouldGetTypeFromJSDoc(node)) {
                diags.push(createDiagnosticForNode(node.name || node, Diagnostics.JSDoc_types_may_be_moved_to_TypeScript_types));
            }
        }

        if (canBeConvertedToAsync(node)) {
            addConvertToAsyncFunctionDiagnostics(node, checker, diags);
        }
        node.forEachChild(check);
    }
}

// convertToEsModule only works on top-level, so don't trigger it if commonjs code only appears in nested scopes.
/**
 * Determines if a given source file contains top level CommonJS statements.
 * @param {SourceFile} sourceFile - The source file to check.
 * @returns {boolean} - True if the source file contains top level CommonJS statements, false otherwise.
 */
function containsTopLevelCommonjs(sourceFile: SourceFile): boolean {
    return sourceFile.statements.some(statement => {
        switch (statement.kind) {
            case SyntaxKind.VariableStatement:
                return (statement as VariableStatement).declarationList.declarations.some(decl =>
                    !!decl.initializer && isRequireCall(propertyAccessLeftHandSide(decl.initializer), /*requireStringLiteralLikeArgument*/ true));
            case SyntaxKind.ExpressionStatement: {
                const { expression } = statement as ExpressionStatement;
                if (!isBinaryExpression(expression)) return isRequireCall(expression, /*requireStringLiteralLikeArgument*/ true);
                const kind = getAssignmentDeclarationKind(expression);
                return kind === AssignmentDeclarationKind.ExportsProperty || kind === AssignmentDeclarationKind.ModuleExports;
            }
            default:
                return false;
        }
    });
}

function propertyAccessLeftHandSide(node: Expression): Expression {
    return isPropertyAccessExpression(node) ? propertyAccessLeftHandSide(node.expression) : node;
}

/**
 * Returns the identifier for converting to default import.
 * @param {AnyValidImportOrReExport} node - The import or re-export node to convert.
 * @returns {Identifier|undefined} - The identifier for default import or undefined if not found.
 */
function importNameForConvertToDefaultImport(node: AnyValidImportOrReExport): Identifier | undefined {
    switch (node.kind) {
        case SyntaxKind.ImportDeclaration:
            const { importClause, moduleSpecifier } = node;
            return importClause && !importClause.name && importClause.namedBindings && importClause.namedBindings.kind === SyntaxKind.NamespaceImport && isStringLiteral(moduleSpecifier)
                ? importClause.namedBindings.name
                : undefined;
        case SyntaxKind.ImportEqualsDeclaration:
            return node.name;
        default:
            return undefined;
    }
}

function addConvertToAsyncFunctionDiagnostics(node: FunctionLikeDeclaration, checker: TypeChecker, diags: DiagnosticWithLocation[]): void {
    // need to check function before checking map so that deeper levels of nested callbacks are checked
    if (isConvertibleFunction(node, checker) && !visitedNestedConvertibleFunctions.has(getKeyFromNode(node))) {
        diags.push(createDiagnosticForNode(
            !node.name && isVariableDeclaration(node.parent) && isIdentifier(node.parent.name) ? node.parent.name : node,
            Diagnostics.This_may_be_converted_to_an_async_function));
    }
}

function isConvertibleFunction(node: FunctionLikeDeclaration, checker: TypeChecker) {
    return !isAsyncFunction(node) &&
        node.body &&
        isBlock(node.body) &&
        hasReturnStatementWithPromiseHandler(node.body, checker) &&
        returnsPromise(node, checker);
}

/** @internal */
export function returnsPromise(node: FunctionLikeDeclaration, checker: TypeChecker): boolean {
    const signature = checker.getSignatureFromDeclaration(node);
    const returnType = signature ? checker.getReturnTypeOfSignature(signature) : undefined;
    return !!returnType && !!checker.getPromisedTypeOfPromise(returnType);
}

function getErrorNodeFromCommonJsIndicator(commonJsModuleIndicator: Node): Node {
    return isBinaryExpression(commonJsModuleIndicator) ? commonJsModuleIndicator.left : commonJsModuleIndicator;
}

function hasReturnStatementWithPromiseHandler(body: Block, checker: TypeChecker): boolean {
    return !!forEachReturnStatement(body, statement => isReturnStatementWithFixablePromiseHandler(statement, checker));
}

/** @internal */
export function isReturnStatementWithFixablePromiseHandler(node: Node, checker: TypeChecker): node is ReturnStatement & { expression: CallExpression } {
    return isReturnStatement(node) && !!node.expression && isFixablePromiseHandler(node.expression, checker);
}

// Should be kept up to date with transformExpression in convertToAsyncFunction.ts
/**
 * Determines if a given node is a fixable promise handler.
 *
 * @param {Node} node - The node to check.
 * @param {TypeChecker} checker - The type checker to use.
 * @returns {boolean} - True if the node is a fixable promise handler, false otherwise.
 *
 * @remarks
 * A fixable promise handler is a function that is passed as a handler to a Promise method, and that can be fixed by adding a return statement or changing the return type.
 */
export function isFixablePromiseHandler(node: Node, checker: TypeChecker): boolean {
    // ensure outermost call exists and is a promise handler
    if (!isPromiseHandler(node) || !hasSupportedNumberOfArguments(node) || !node.arguments.every(arg => isFixablePromiseArgument(arg, checker))) {
        return false;
    }

    // ensure all chained calls are valid
    let currentNode = node.expression.expression;
    while (isPromiseHandler(currentNode) || isPropertyAccessExpression(currentNode)) {
        if (isCallExpression(currentNode)) {
            if (!hasSupportedNumberOfArguments(currentNode) || !currentNode.arguments.every(arg => isFixablePromiseArgument(arg, checker))) {
                return false;
            }
            currentNode = currentNode.expression.expression;
        }
        else {
            currentNode = currentNode.expression;
        }
    }
    return true;
}

function isPromiseHandler(node: Node): node is CallExpression & { readonly expression: PropertyAccessExpression } {
    return isCallExpression(node) && (
        hasPropertyAccessExpressionWithName(node, "then") ||
        hasPropertyAccessExpressionWithName(node, "catch") ||
        hasPropertyAccessExpressionWithName(node, "finally"));
}

function hasSupportedNumberOfArguments(node: CallExpression & { readonly expression: PropertyAccessExpression }) {
    const name = node.expression.name.text;
    const maxArguments = name === "then" ? 2 : name === "catch" ? 1 : name === "finally" ? 1 : 0;
    if (node.arguments.length > maxArguments) return false;
    if (node.arguments.length < maxArguments) return true;
    return maxArguments === 1 || some(node.arguments, arg => {
        return arg.kind === SyntaxKind.NullKeyword || isIdentifier(arg) && arg.text === "undefined";
    });
}

// should be kept up to date with getTransformationBody in convertToAsyncFunction.ts
/**
 * Determines if the given argument is a fixable Promise argument.
 * @param {Expression} arg - The argument to check.
 * @param {TypeChecker} checker - The TypeChecker to use.
 * @returns {boolean} - True if the argument is fixable, false otherwise.
 */
function isFixablePromiseArgument(arg: Expression, checker: TypeChecker): boolean {
    switch (arg.kind) {
        case SyntaxKind.FunctionDeclaration:
        case SyntaxKind.FunctionExpression:
            const functionFlags = getFunctionFlags(arg as FunctionDeclaration | FunctionExpression);
            if (functionFlags & FunctionFlags.Generator) {
                return false;
            }
            // falls through
        case SyntaxKind.ArrowFunction:
            visitedNestedConvertibleFunctions.set(getKeyFromNode(arg as FunctionLikeDeclaration), true);
            // falls through
        case SyntaxKind.NullKeyword:
            return true;
        case SyntaxKind.Identifier:
        case SyntaxKind.PropertyAccessExpression: {
            const symbol = checker.getSymbolAtLocation(arg);
            if (!symbol) {
                return false;
            }
            return checker.isUndefinedSymbol(symbol) ||
                some(skipAlias(symbol, checker).declarations, d => isFunctionLike(d) || hasInitializer(d) && !!d.initializer && isFunctionLike(d.initializer));
        }
        default:
            return false;
    }
}

function getKeyFromNode(exp: FunctionLikeDeclaration) {
    return `${exp.pos.toString()}:${exp.end.toString()}`;
}

/**
 * Determines if a given Node can be converted to a class.
 * @param {Node} node - The Node to check.
 * @param {TypeChecker} checker - The TypeChecker to use.
 * @returns {boolean} - True if the Node can be converted to a class, false otherwise.
 */
function canBeConvertedToClass(node: Node, checker: TypeChecker): boolean {
    if (isFunctionExpression(node)) {
        if (isVariableDeclaration(node.parent) && node.symbol.members?.size) {
            return true;
        }

        const symbol = checker.getSymbolOfExpando(node, /*allowDeclaration*/ false);
        return !!(symbol && (symbol.exports?.size || symbol.members?.size));
    }

    if (isFunctionDeclaration(node)) {
        return !!node.symbol.members?.size;
    }

    return false;
}

/**
 * Determines if a given node can be converted to an async function.
 * @param {Node} node - The node to check.
 * @returns {boolean} - True if the node can be converted to an async function, false otherwise.
 * @internal
 */
export function canBeConvertedToAsync(node: Node): node is FunctionDeclaration | MethodDeclaration | FunctionExpression | ArrowFunction {
    switch (node.kind) {
        case SyntaxKind.FunctionDeclaration:
        case SyntaxKind.MethodDeclaration:
        case SyntaxKind.FunctionExpression:
        case SyntaxKind.ArrowFunction:
            return true;
        default:
            return false;
    }
}
