import {
    ApplicableRefactorInfo,
    ArrowFunction,
    Block,
    ConciseBody,
    copyComments,
    copyTrailingAsLeadingComments,
    Debug,
    Diagnostics,
    emptyArray,
    factory,
    FileTextChanges,
    FindAllReferences,
    first,
    forEachChild,
    FunctionExpression,
    getCombinedModifierFlags,
    getContainingFunction,
    getEffectiveModifierFlags,
    getLocaleSpecificMessage,
    getTokenAtPosition,
    Identifier,
    isArrowFunction,
    isClassLike,
    isExpression,
    isFunctionDeclaration,
    isFunctionExpression,
    isIdentifier,
    isReturnStatement,
    isThis,
    isVariableDeclaration,
    isVariableDeclarationInVariableStatement,
    isVariableDeclarationList,
    isVariableStatement,
    length,
    ModifierFlags,
    Node,
    Program,
    rangeContainsRange,
    RefactorActionInfo,
    RefactorContext,
    RefactorEditInfo,
    ReturnStatement,
    setTextRange,
    SourceFile,
    Statement,
    suppressLeadingAndTrailingTrivia,
    suppressLeadingTrivia,
    SyntaxKind,
    textChanges,
    TypeChecker,
    VariableDeclaration,
    VariableDeclarationList,
    VariableStatement,
} from "../_namespaces/ts";
import {
    refactorKindBeginsWith,
    registerRefactor,
} from "../_namespaces/ts.refactor";

const refactorName = "Convert arrow function or function expression";
const refactorDescription = getLocaleSpecificMessage(Diagnostics.Convert_arrow_function_or_function_expression);

const toAnonymousFunctionAction = {
    name: "Convert to anonymous function",
    description: getLocaleSpecificMessage(Diagnostics.Convert_to_anonymous_function),
    kind: "refactor.rewrite.function.anonymous",
};
const toNamedFunctionAction = {
    name: "Convert to named function",
    description: getLocaleSpecificMessage(Diagnostics.Convert_to_named_function),
    kind: "refactor.rewrite.function.named",
};
const toArrowFunctionAction = {
    name: "Convert to arrow function",
    description: getLocaleSpecificMessage(Diagnostics.Convert_to_arrow_function),
    kind: "refactor.rewrite.function.arrow",
};
registerRefactor(refactorName, {
    kinds: [
        toAnonymousFunctionAction.kind,
        toNamedFunctionAction.kind,
        toArrowFunctionAction.kind
    ],
    getEditsForAction: getRefactorEditsToConvertFunctionExpressions,
    getAvailableActions: getRefactorActionsToConvertFunctionExpressions
});

interface FunctionInfo {
    readonly selectedVariableDeclaration: boolean;
    readonly func: FunctionExpression | ArrowFunction;
}

interface VariableInfo {
    readonly variableDeclaration: VariableDeclaration;
    readonly variableDeclarationList: VariableDeclarationList;
    readonly statement: VariableStatement;
    readonly name: Identifier;
}

/**
 * Returns an array of applicable refactor actions based on the provided RefactorContext object.
 * @param {RefactorContext} context - The RefactorContext object containing information about the file, start position, program, and kind.
 * @returns {readonly ApplicableRefactorInfo[]} An array of applicable refactor actions.
 */
function getRefactorActionsToConvertFunctionExpressions(context: RefactorContext): readonly ApplicableRefactorInfo[] {
    const { file, startPosition, program, kind } = context;
    const info = getFunctionInfo(file, startPosition, program);

    if (!info) return emptyArray;
    const { selectedVariableDeclaration, func } = info;
    const possibleActions: RefactorActionInfo[] = [];
    const errors: RefactorActionInfo[] = [];
    if (refactorKindBeginsWith(toNamedFunctionAction.kind, kind)) {
        const error = selectedVariableDeclaration || (isArrowFunction(func) && isVariableDeclaration(func.parent)) ?
            undefined : getLocaleSpecificMessage(Diagnostics.Could_not_convert_to_named_function);
        if (error) {
            errors.push({ ...toNamedFunctionAction, notApplicableReason: error });
        }
        else {
            possibleActions.push(toNamedFunctionAction);
        }
    }

    if (refactorKindBeginsWith(toAnonymousFunctionAction.kind, kind)) {
        const error = !selectedVariableDeclaration && isArrowFunction(func) ?
            undefined: getLocaleSpecificMessage(Diagnostics.Could_not_convert_to_anonymous_function);
        if (error) {
            errors.push({ ...toAnonymousFunctionAction, notApplicableReason: error });
        }
        else {
            possibleActions.push(toAnonymousFunctionAction);
        }
    }

    if (refactorKindBeginsWith(toArrowFunctionAction.kind, kind)) {
        const error = isFunctionExpression(func) ? undefined : getLocaleSpecificMessage(Diagnostics.Could_not_convert_to_arrow_function);
        if (error) {
            errors.push({ ...toArrowFunctionAction, notApplicableReason: error });
        }
        else {
            possibleActions.push(toArrowFunctionAction);
        }
    }

    return [{
        name: refactorName,
        description: refactorDescription,
        actions: possibleActions.length === 0 && context.preferences.provideRefactorNotApplicableReason ?
            errors : possibleActions
    }];
}

/**
 * Returns RefactorEditInfo object or undefined based on the actionName parameter passed.
 * @param {RefactorContext} context - The context object containing information about the file, startPosition and program.
 * @param {string} actionName - The name of the action to be performed.
 * @returns {RefactorEditInfo | undefined} - RefactorEditInfo object or undefined.
 */
function getRefactorEditsToConvertFunctionExpressions(context: RefactorContext, actionName: string): RefactorEditInfo | undefined {
    const { file, startPosition, program } = context;
    const info = getFunctionInfo(file, startPosition, program);

    if (!info) return undefined;
    const { func } = info;
    const edits: FileTextChanges[] = [];

    switch (actionName) {
        case toAnonymousFunctionAction.name:
            edits.push(...getEditInfoForConvertToAnonymousFunction(context, func));
            break;

        case toNamedFunctionAction.name:
            const variableInfo = getVariableInfo(func);
            if (!variableInfo) return undefined;

            edits.push(...getEditInfoForConvertToNamedFunction(context, func, variableInfo));
            break;

        case toArrowFunctionAction.name:
            if (!isFunctionExpression(func)) return undefined;
            edits.push(...getEditInfoForConvertToArrowFunction(context, func));
            break;

        default:
            return Debug.fail("invalid action");
    }

    return { renameFilename: undefined, renameLocation: undefined, edits };
}

function containingThis(node: Node): boolean {
    let containsThis = false;
    node.forEachChild(function checkThis(child) {

        if (isThis(child)) {
            containsThis = true;
            return;
        }

        if (!isClassLike(child) && !isFunctionDeclaration(child) && !isFunctionExpression(child)) {
            forEachChild(child, checkThis);
        }
    });

    return containsThis;
}

function getFunctionInfo(file: SourceFile, startPosition: number, program: Program): FunctionInfo | undefined {
    const token = getTokenAtPosition(file, startPosition);
    const typeChecker = program.getTypeChecker();
    const func = tryGetFunctionFromVariableDeclaration(file, typeChecker, token.parent);
    if (func && !containingThis(func.body) && !typeChecker.containsArgumentsReference(func)) {
        return { selectedVariableDeclaration: true, func };
    }

    const maybeFunc = getContainingFunction(token);
    if (
        maybeFunc &&
        (isFunctionExpression(maybeFunc) || isArrowFunction(maybeFunc)) &&
        !rangeContainsRange(maybeFunc.body, token) &&
        !containingThis(maybeFunc.body) &&
        !typeChecker.containsArgumentsReference(maybeFunc)
    ) {
        if (isFunctionExpression(maybeFunc) && isFunctionReferencedInFile(file, typeChecker, maybeFunc)) return undefined;
        return { selectedVariableDeclaration: false, func: maybeFunc };
    }

    return undefined;
}

function isSingleVariableDeclaration(parent: Node): parent is VariableDeclarationList {
    return isVariableDeclaration(parent) || (isVariableDeclarationList(parent) && parent.declarations.length === 1);
}

/**
 * Tries to retrieve a function from a variable declaration.
 * @param {SourceFile} sourceFile - The source file containing the variable declaration.
 * @param {TypeChecker} typeChecker - The type checker for the source file.
 * @param {Node} parent - The parent node of the variable declaration.
 * @returns {ArrowFunction | FunctionExpression | undefined} The retrieved function or undefined if not found.
 */
function tryGetFunctionFromVariableDeclaration(sourceFile: SourceFile, typeChecker: TypeChecker, parent: Node): ArrowFunction | FunctionExpression | undefined {
    if (!isSingleVariableDeclaration(parent)) {
        return undefined;
    }
    const variableDeclaration = isVariableDeclaration(parent) ? parent : first(parent.declarations);
    const initializer = variableDeclaration.initializer;
    if (initializer && (isArrowFunction(initializer) || isFunctionExpression(initializer) && !isFunctionReferencedInFile(sourceFile, typeChecker, initializer))) {
        return initializer;
    }
    return undefined;
}

/**
 * Converts a concise body to a block.
 * @param {ConciseBody} body - The concise body to convert.
 * @returns {Block} The resulting block.
 */
function convertToBlock(body: ConciseBody): Block {
    if (isExpression(body)) {
        const returnStatement = factory.createReturnStatement(body);
        const file = body.getSourceFile();
        setTextRange(returnStatement, body);
        suppressLeadingAndTrailingTrivia(returnStatement);
        copyTrailingAsLeadingComments(body, returnStatement, file, /*commentKind*/ undefined, /*hasTrailingNewLine*/ true);
        return factory.createBlock([returnStatement], /*multiLine*/ true);
    }
    else {
        return body;
    }
}

/**
 * Returns information about a variable declaration within a function expression or arrow function.
 * @param func The function expression or arrow function to analyze.
 * @returns An object containing information about the variable declaration, or undefined if the function is not a variable declaration within a variable statement.
 */
function getVariableInfo(func: FunctionExpression | ArrowFunction): VariableInfo | undefined {
    const variableDeclaration = func.parent;
    if (!isVariableDeclaration(variableDeclaration) || !isVariableDeclarationInVariableStatement(variableDeclaration)) return undefined;

    const variableDeclarationList = variableDeclaration.parent;
    const statement = variableDeclarationList.parent;
    if (!isVariableDeclarationList(variableDeclarationList) || !isVariableStatement(statement) || !isIdentifier(variableDeclaration.name)) return undefined;

    return { variableDeclaration, variableDeclarationList, statement, name: variableDeclaration.name };
}

function getEditInfoForConvertToAnonymousFunction(context: RefactorContext, func: FunctionExpression | ArrowFunction): FileTextChanges[] {
    const { file } = context;
    const body = convertToBlock(func.body);
    const newNode = factory.createFunctionExpression(func.modifiers, func.asteriskToken, /*name*/ undefined, func.typeParameters, func.parameters, func.type, body);
    return textChanges.ChangeTracker.with(context, t => t.replaceNode(file, func, newNode));
}

/**
 * Converts a given FunctionExpression or ArrowFunction to a FunctionDeclaration and returns FileTextChanges array.
 * @param {RefactorContext} context - The context object for the refactoring.
 * @param {FunctionExpression | ArrowFunction} func - The function to be converted.
 * @param {VariableInfo} variableInfo - The variable information object.
 * @returns {FileTextChanges[]} - The array of file text changes.
 */
function getEditInfoForConvertToNamedFunction(context: RefactorContext, func: FunctionExpression | ArrowFunction, variableInfo: VariableInfo): FileTextChanges[] {
    const { file } = context;
    const body = convertToBlock(func.body);

    const { variableDeclaration, variableDeclarationList, statement, name } = variableInfo;
    suppressLeadingTrivia(statement);

    const modifiersFlags = (getCombinedModifierFlags(variableDeclaration) & ModifierFlags.Export) | getEffectiveModifierFlags(func);
    const modifiers = factory.createModifiersFromModifierFlags(modifiersFlags);
    const newNode = factory.createFunctionDeclaration(length(modifiers) ? modifiers : undefined, func.asteriskToken, name, func.typeParameters, func.parameters, func.type, body);

    if (variableDeclarationList.declarations.length === 1) {
        return textChanges.ChangeTracker.with(context, t => t.replaceNode(file, statement, newNode));
    }
    else {
        return textChanges.ChangeTracker.with(context, t => {
            t.delete(file, variableDeclaration);
            t.insertNodeAfter(file, statement, newNode);
        });
    }
}

/**
 * Returns an array of FileTextChanges after converting a given FunctionExpression to an Arrow Function.
 * @param {RefactorContext} context - The context of the refactoring.
 * @param {FunctionExpression} func - The FunctionExpression to be converted.
 * @returns {FileTextChanges[]} - An array of FileTextChanges after the conversion.
 */
function getEditInfoForConvertToArrowFunction(context: RefactorContext, func: FunctionExpression): FileTextChanges[] {
    const { file } = context;
    const statements = func.body.statements;
    const head = statements[0];
    let body: ConciseBody;

    if (canBeConvertedToExpression(func.body, head)) {
        body = head.expression!;
        suppressLeadingAndTrailingTrivia(body);
        copyComments(head, body);
    }
    else {
        body = func.body;
    }

    const newNode = factory.createArrowFunction(func.modifiers, func.typeParameters, func.parameters, func.type, factory.createToken(SyntaxKind.EqualsGreaterThanToken), body);
    return textChanges.ChangeTracker.with(context, t => t.replaceNode(file, func, newNode));
}

function canBeConvertedToExpression(body: Block, head: Statement): head is ReturnStatement {
    return body.statements.length === 1 && ((isReturnStatement(head) && !!head.expression));
}

function isFunctionReferencedInFile(sourceFile: SourceFile, typeChecker: TypeChecker, node: FunctionExpression): boolean {
    return !!node.name && FindAllReferences.Core.isSymbolReferencedInFile(node.name, typeChecker, sourceFile);
}
