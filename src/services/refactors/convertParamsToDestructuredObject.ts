import {
    addEmitFlags,
    ApplicableRefactorInfo,
    ArrowFunction,
    BindingElement,
    CallExpression,
    CancellationToken,
    CheckFlags,
    ClassDeclaration,
    ClassExpression,
    compareValues,
    ConstructorDeclaration,
    contains,
    copyComments,
    Debug,
    deduplicate,
    Diagnostics,
    ElementAccessExpression,
    EmitFlags,
    emptyArray,
    equateValues,
    every,
    Expression,
    factory,
    FindAllReferences,
    findAncestor,
    findChildOfKind,
    findModifier,
    first,
    flatMap,
    FunctionBody,
    FunctionDeclaration,
    FunctionExpression,
    FunctionLikeDeclaration,
    getCheckFlags,
    getContainingFunctionDeclaration,
    getContainingObjectLiteralElement,
    getLocaleSpecificMessage,
    getMeaningFromLocation,
    getSourceFileOfNode,
    getSymbolTarget,
    getSynthesizedDeepClone,
    getTextOfIdentifierOrLiteral,
    getTouchingToken,
    getTypeNodeIfAccessible,
    Identifier,
    isCallOrNewExpression,
    isClassDeclaration,
    isConstructorDeclaration,
    isDeclaration,
    isElementAccessExpression,
    isExportAssignment,
    isExportSpecifier,
    isExpressionWithTypeArgumentsInClassExtendsClause,
    isFunctionLikeDeclaration,
    isIdentifier,
    isImportClause,
    isImportEqualsDeclaration,
    isImportSpecifier,
    isInterfaceDeclaration,
    isJSDocNode,
    isMethodSignature,
    isNamespaceImport,
    isNewExpressionTarget,
    isObjectLiteralExpression,
    isPropertyAccessExpression,
    isPropertyAssignment,
    isRestParameter,
    isSourceFileJS,
    isThis,
    isTypeLiteralNode,
    isVarConst,
    isVariableDeclaration,
    LanguageServiceHost,
    last,
    map,
    MethodDeclaration,
    MethodSignature,
    Modifier,
    NewExpression,
    Node,
    NodeArray,
    ObjectLiteralElementLike,
    ObjectLiteralExpression,
    ParameterDeclaration,
    Program,
    PropertyAccessExpression,
    PropertyAssignment,
    PropertySignature,
    rangeContainsRange,
    RefactorContext,
    RefactorEditInfo,
    SemanticMeaning,
    ShorthandPropertyAssignment,
    sortAndDeduplicate,
    SourceFile,
    suppressLeadingAndTrailingTrivia,
    Symbol,
    SyntaxKind,
    textChanges,
    tryCast,
    TypeChecker,
    TypeLiteralNode,
    TypeNode,
    VariableDeclaration,
} from "../_namespaces/ts";
import { registerRefactor } from "../_namespaces/ts.refactor";

const refactorName = "Convert parameters to destructured object";
const minimumParameterLength = 1;
const refactorDescription = getLocaleSpecificMessage(Diagnostics.Convert_parameters_to_destructured_object);

const toDestructuredAction = {
    name: refactorName,
    description: refactorDescription,
    kind: "refactor.rewrite.parameters.toDestructured"
};
registerRefactor(refactorName, {
    kinds: [toDestructuredAction.kind],
    getEditsForAction: getRefactorEditsToConvertParametersToDestructuredObject,
    getAvailableActions: getRefactorActionsToConvertParametersToDestructuredObject
});

/**
 * Returns an array of applicable refactor actions to convert function parameters to a destructured object.
 * @param {RefactorContext} context - The context object containing information about the file and starting position.
 * @returns {readonly ApplicableRefactorInfo[]} - An array of applicable refactor actions.
 * @remarks This function checks if the file is a JavaScript file and returns an empty array if it is. It also retrieves the function declaration at the starting position and returns an array of applicable refactor actions.
 */
function getRefactorActionsToConvertParametersToDestructuredObject(context: RefactorContext): readonly ApplicableRefactorInfo[] {
    const { file, startPosition } = context;
    const isJSFile = isSourceFileJS(file);
    if (isJSFile) return emptyArray; // TODO: GH#30113
    const functionDeclaration = getFunctionDeclarationAtPosition(file, startPosition, context.program.getTypeChecker());
    if (!functionDeclaration) return emptyArray;

    return [{
        name: refactorName,
        description: refactorDescription,
        actions: [toDestructuredAction]
    }];
}

/**
 * Retrieves the refactor edits needed to convert function parameters to a destructured object.
 * @param {RefactorContext} context - The context of the refactor.
 * @param {string} actionName - The name of the action to perform.
 * @returns {RefactorEditInfo | undefined} - The edits needed to perform the refactor, or undefined if the function declaration or cancellation token is missing.
 */
function getRefactorEditsToConvertParametersToDestructuredObject(context: RefactorContext, actionName: string): RefactorEditInfo | undefined {
    Debug.assert(actionName === refactorName, "Unexpected action name");
    const { file, startPosition, program, cancellationToken, host } = context;
    const functionDeclaration = getFunctionDeclarationAtPosition(file, startPosition, program.getTypeChecker());
    if (!functionDeclaration || !cancellationToken) return undefined;

    const groupedReferences = getGroupedReferences(functionDeclaration, program, cancellationToken);
    if (groupedReferences.valid) {
        const edits = textChanges.ChangeTracker.with(context, t => doChange(file, program, host, t, functionDeclaration, groupedReferences));
        return { renameFilename: undefined, renameLocation: undefined, edits };
    }

    return { edits: [] }; // TODO: GH#30113
}

function doChange(
    sourceFile: SourceFile,
    program: Program,
    host: LanguageServiceHost,
    changes: textChanges.ChangeTracker,
    functionDeclaration: ValidFunctionDeclaration,
    groupedReferences: GroupedReferences): void {
    const signature = groupedReferences.signature;
    const newFunctionDeclarationParams = map(createNewParameters(functionDeclaration, program, host), param => getSynthesizedDeepClone(param));

    if (signature) {
        const newSignatureParams = map(createNewParameters(signature, program, host), param => getSynthesizedDeepClone(param));
        replaceParameters(signature, newSignatureParams);
    }
    replaceParameters(functionDeclaration, newFunctionDeclarationParams);

    const functionCalls = sortAndDeduplicate(groupedReferences.functionCalls, /*comparer*/ (a, b) => compareValues(a.pos, b.pos));
    for (const call of functionCalls) {
        if (call.arguments && call.arguments.length) {
            const newArgument = getSynthesizedDeepClone(createNewArgument(functionDeclaration, call.arguments), /*includeTrivia*/ true);
            changes.replaceNodeRange(
                getSourceFileOfNode(call),
                first(call.arguments),
                last(call.arguments),
                newArgument,
                { leadingTriviaOption: textChanges.LeadingTriviaOption.IncludeAll, trailingTriviaOption: textChanges.TrailingTriviaOption.Include });
        }
    }

    function replaceParameters(declarationOrSignature: ValidFunctionDeclaration | ValidMethodSignature, parameterDeclarations: ParameterDeclaration[]) {
        changes.replaceNodeRangeWithNodes(
            sourceFile,
            first(declarationOrSignature.parameters),
            last(declarationOrSignature.parameters),
            parameterDeclarations,
            {
                joiner: ", ",
                // indentation is set to 0 because otherwise the object parameter will be indented if there is a `this` parameter
                indentation: 0,
                leadingTriviaOption: textChanges.LeadingTriviaOption.IncludeAll,
                trailingTriviaOption: textChanges.TrailingTriviaOption.Include
            });
    }
}

function getGroupedReferences(functionDeclaration: ValidFunctionDeclaration, program: Program, cancellationToken: CancellationToken): GroupedReferences {
    const functionNames = getFunctionNames(functionDeclaration);
    const classNames = isConstructorDeclaration(functionDeclaration) ? getClassNames(functionDeclaration) : [];
    const names = deduplicate([...functionNames, ...classNames], equateValues);
    const checker = program.getTypeChecker();

    const references = flatMap(names, /*mapfn*/ name => FindAllReferences.getReferenceEntriesForNode(-1, name, program, program.getSourceFiles(), cancellationToken));
    const groupedReferences = groupReferences(references);

    if (!every(groupedReferences.declarations, /*callback*/ decl => contains(names, decl))) {
        groupedReferences.valid = false;
    }

    return groupedReferences;

    function groupReferences(referenceEntries: readonly FindAllReferences.Entry[]): GroupedReferences {
        const classReferences: ClassReferences = { accessExpressions: [], typeUsages: [] };
        const groupedReferences: GroupedReferences = { functionCalls: [], declarations: [], classReferences, valid: true };
        const functionSymbols = map(functionNames, getSymbolTargetAtLocation);
        const classSymbols = map(classNames, getSymbolTargetAtLocation);
        const isConstructor = isConstructorDeclaration(functionDeclaration);
        const contextualSymbols = map(functionNames, name => getSymbolForContextualType(name, checker));

        for (const entry of referenceEntries) {
            if (entry.kind === FindAllReferences.EntryKind.Span) {
                groupedReferences.valid = false;
                continue;
            }

            /* Declarations in object literals may be implementations of method signatures which have a different symbol from the declaration
            For example:
                interface IFoo { m(a: number): void }
                const foo: IFoo = { m(a: number): void {} }
            In these cases we get the symbol for the signature from the contextual type.
            */
            if (contains(contextualSymbols, getSymbolTargetAtLocation(entry.node))) {
                if (isValidMethodSignature(entry.node.parent)) {
                    groupedReferences.signature = entry.node.parent;
                    continue;
                }
                const call = entryToFunctionCall(entry);
                if (call) {
                    groupedReferences.functionCalls.push(call);
                    continue;
                }
            }

            const contextualSymbol = getSymbolForContextualType(entry.node, checker);
            if (contextualSymbol && contains(contextualSymbols, contextualSymbol)) {
                const decl = entryToDeclaration(entry);
                if (decl) {
                    groupedReferences.declarations.push(decl);
                    continue;
                }
            }

            /* We compare symbols because in some cases find all references wil return a reference that may or may not be to the refactored function.
            Example from the refactorConvertParamsToDestructuredObject_methodCallUnion.ts test:
                class A { foo(a: number, b: number) { return a + b; } }
                class B { foo(c: number, d: number) { return c + d; } }
                declare const ab: A | B;
                ab.foo(1, 2);
            Find all references will return `ab.foo(1, 2)` as a reference to A's `foo` but we could be calling B's `foo`.
            When looking for constructor calls, however, the symbol on the constructor call reference is going to be the corresponding class symbol.
            So we need to add a special case for this because when calling a constructor of a class through one of its subclasses,
            the symbols are going to be different.
            */
            if (contains(functionSymbols, getSymbolTargetAtLocation(entry.node)) || isNewExpressionTarget(entry.node)) {
                const importOrExportReference = entryToImportOrExport(entry);
                if (importOrExportReference) {
                    continue;
                }
                const decl = entryToDeclaration(entry);
                if (decl) {
                    groupedReferences.declarations.push(decl);
                    continue;
                }

                const call = entryToFunctionCall(entry);
                if (call) {
                    groupedReferences.functionCalls.push(call);
                    continue;
                }
            }
            // if the refactored function is a constructor, we must also check if the references to its class are valid
            if (isConstructor && contains(classSymbols, getSymbolTargetAtLocation(entry.node))) {
                const importOrExportReference = entryToImportOrExport(entry);
                if (importOrExportReference) {
                    continue;
                }

                const decl = entryToDeclaration(entry);
                if (decl) {
                    groupedReferences.declarations.push(decl);
                    continue;
                }

                const accessExpression = entryToAccessExpression(entry);
                if (accessExpression) {
                    classReferences.accessExpressions.push(accessExpression);
                    continue;
                }

                // Only class declarations are allowed to be used as a type (in a heritage clause),
                // otherwise `findAllReferences` might not be able to track constructor calls.
                if (isClassDeclaration(functionDeclaration.parent)) {
                    const type = entryToType(entry);
                    if (type) {
                        classReferences.typeUsages.push(type);
                        continue;
                    }
                }
            }
            groupedReferences.valid = false;
        }

        return groupedReferences;
    }

    function getSymbolTargetAtLocation(node: Node) {
        const symbol = checker.getSymbolAtLocation(node);
        return symbol && getSymbolTarget(symbol, checker);
    }
}

/**
 * Retrieves the symbol for the contextual type of a given node, provided it is not a union or intersection.
 * @param {Node} node - The node to retrieve the contextual type symbol for.
 * @param {TypeChecker} checker - The TypeChecker instance to use for retrieving the contextual type.
 * @returns {Symbol | undefined} The symbol for the contextual type of the node, or undefined if it is a union or intersection or if no symbol is found.
 */
function getSymbolForContextualType(node: Node, checker: TypeChecker): Symbol | undefined {
    const element = getContainingObjectLiteralElement(node);
    if (element) {
        const contextualType = checker.getContextualTypeForObjectLiteralElement(element as ObjectLiteralElementLike);
        const symbol = contextualType?.getSymbol();
        if (symbol && !(getCheckFlags(symbol) & CheckFlags.Synthetic)) {
            return symbol;
        }
    }
}

/**
 * Returns a Node or undefined based on the given FindAllReferences.NodeEntry object.
 * @param {FindAllReferences.NodeEntry} entry - The NodeEntry object to be processed.
 * @returns {Node | undefined} - Returns a Node or undefined.
 */
function entryToImportOrExport(entry: FindAllReferences.NodeEntry): Node | undefined {
    const node = entry.node;

    if (isImportSpecifier(node.parent)
        || isImportClause(node.parent)
        || isImportEqualsDeclaration(node.parent)
        || isNamespaceImport(node.parent)) {
        return node;
    }

    if (isExportSpecifier(node.parent) || isExportAssignment(node.parent)) {
        return node;
    }
    return undefined;
}

function entryToDeclaration(entry: FindAllReferences.NodeEntry): Node | undefined {
    if (isDeclaration(entry.node.parent)) {
        return entry.node;
    }
    return undefined;
}

/**
 * Given a NodeEntry, this function attempts to find the corresponding CallExpression or NewExpression that references the node.
 * @param {FindAllReferences.NodeEntry} entry - The NodeEntry to search for references of.
 * @returns {CallExpression | NewExpression | undefined} - The corresponding CallExpression or NewExpression, or undefined if not found.
 */
function entryToFunctionCall(entry: FindAllReferences.NodeEntry): CallExpression | NewExpression | undefined {
    if (entry.node.parent) {
        const functionReference = entry.node;
        const parent = functionReference.parent;
        switch (parent.kind) {
            // foo(...) or super(...) or new Foo(...)
            case SyntaxKind.CallExpression:
            case SyntaxKind.NewExpression:
                const callOrNewExpression = tryCast(parent, isCallOrNewExpression);
                if (callOrNewExpression && callOrNewExpression.expression === functionReference) {
                    return callOrNewExpression;
                }
                break;
            // x.foo(...)
            case SyntaxKind.PropertyAccessExpression:
                const propertyAccessExpression = tryCast(parent, isPropertyAccessExpression);
                if (propertyAccessExpression && propertyAccessExpression.parent && propertyAccessExpression.name === functionReference) {
                    const callOrNewExpression = tryCast(propertyAccessExpression.parent, isCallOrNewExpression);
                    if (callOrNewExpression && callOrNewExpression.expression === propertyAccessExpression) {
                        return callOrNewExpression;
                    }
                }
                break;
            // x["foo"](...)
            case SyntaxKind.ElementAccessExpression:
                const elementAccessExpression = tryCast(parent, isElementAccessExpression);
                if (elementAccessExpression && elementAccessExpression.parent && elementAccessExpression.argumentExpression === functionReference) {
                    const callOrNewExpression = tryCast(elementAccessExpression.parent, isCallOrNewExpression);
                    if (callOrNewExpression && callOrNewExpression.expression === elementAccessExpression) {
                        return callOrNewExpression;
                    }
                }
                break;
        }
    }
    return undefined;
}

/**
 * Returns an ElementAccessExpression or PropertyAccessExpression based on the provided NodeEntry.
 * @param {FindAllReferences.NodeEntry} entry - The NodeEntry to be used.
 * @returns {ElementAccessExpression | PropertyAccessExpression | undefined} - The resulting access expression or undefined if none is found.
 */
function entryToAccessExpression(entry: FindAllReferences.NodeEntry): ElementAccessExpression | PropertyAccessExpression | undefined {
    if (entry.node.parent) {
        const reference = entry.node;
        const parent = reference.parent;
        switch (parent.kind) {
            // `C.foo`
            case SyntaxKind.PropertyAccessExpression:
                const propertyAccessExpression = tryCast(parent, isPropertyAccessExpression);
                if (propertyAccessExpression && propertyAccessExpression.expression === reference) {
                    return propertyAccessExpression;
                }
                break;
            // `C["foo"]`
            case SyntaxKind.ElementAccessExpression:
                const elementAccessExpression = tryCast(parent, isElementAccessExpression);
                if (elementAccessExpression && elementAccessExpression.expression === reference) {
                    return elementAccessExpression;
                }
                break;
        }
    }
    return undefined;
}

function entryToType(entry: FindAllReferences.NodeEntry): Node | undefined {
    const reference = entry.node;
    if (getMeaningFromLocation(reference) === SemanticMeaning.Type || isExpressionWithTypeArgumentsInClassExtendsClause(reference.parent)) {
        return reference;
    }
    return undefined;
}

/**
 * Retrieves the function declaration at a given position in a source file.
 * @param {SourceFile} file - The source file to search in.
 * @param {number} startPosition - The position to search for the function declaration.
 * @param {TypeChecker} checker - The type checker to use for validation.
 * @returns {ValidFunctionDeclaration | undefined} The function declaration at the given position, or undefined if not found.
 */
function getFunctionDeclarationAtPosition(file: SourceFile, startPosition: number, checker: TypeChecker): ValidFunctionDeclaration | undefined {
    const node = getTouchingToken(file, startPosition);
    const functionDeclaration = getContainingFunctionDeclaration(node);

    // don't offer refactor on top-level JSDoc
    if (isTopLevelJSDoc(node)) return undefined;

    if (functionDeclaration
        && isValidFunctionDeclaration(functionDeclaration, checker)
        && rangeContainsRange(functionDeclaration, node)
        && !(functionDeclaration.body && rangeContainsRange(functionDeclaration.body, node))) return functionDeclaration;

    return undefined;
}

function isTopLevelJSDoc(node: Node): boolean {
    const containingJSDoc = findAncestor(node, isJSDocNode);
    if (containingJSDoc) {
        const containingNonJSDoc = findAncestor(containingJSDoc, n => !isJSDocNode(n));
        return !!containingNonJSDoc && isFunctionLikeDeclaration(containingNonJSDoc);
    }
    return false;
}

function isValidMethodSignature(node: Node): node is ValidMethodSignature {
    return isMethodSignature(node) && (isInterfaceDeclaration(node.parent) || isTypeLiteralNode(node.parent));
}

/**
 * Determines if a given function declaration is valid based on its parameters and implementation.
 * @param functionDeclaration The function declaration to check.
 * @param checker The TypeChecker instance to use for type checking.
 * @returns A boolean indicating whether the function declaration is valid or not.
 */
function isValidFunctionDeclaration(
    functionDeclaration: FunctionLikeDeclaration,
    checker: TypeChecker): functionDeclaration is ValidFunctionDeclaration {
    if (!isValidParameterNodeArray(functionDeclaration.parameters, checker)) return false;
    switch (functionDeclaration.kind) {
        case SyntaxKind.FunctionDeclaration:
            return hasNameOrDefault(functionDeclaration) && isSingleImplementation(functionDeclaration, checker);
        case SyntaxKind.MethodDeclaration:
            if (isObjectLiteralExpression(functionDeclaration.parent)) {
                const contextualSymbol = getSymbolForContextualType(functionDeclaration.name, checker);
                // don't offer the refactor when there are multiple signatures since we won't know which ones the user wants to change
                return contextualSymbol?.declarations?.length === 1 && isSingleImplementation(functionDeclaration, checker);
            }
            return isSingleImplementation(functionDeclaration, checker);
        case SyntaxKind.Constructor:
            if (isClassDeclaration(functionDeclaration.parent)) {
                return hasNameOrDefault(functionDeclaration.parent) && isSingleImplementation(functionDeclaration, checker);
            }
            else {
                return isValidVariableDeclaration(functionDeclaration.parent.parent)
                    && isSingleImplementation(functionDeclaration, checker);
            }
        case SyntaxKind.FunctionExpression:
        case SyntaxKind.ArrowFunction:
            return isValidVariableDeclaration(functionDeclaration.parent);
    }
    return false;
}

function isSingleImplementation(functionDeclaration: FunctionLikeDeclaration, checker: TypeChecker): boolean {
    return !!functionDeclaration.body && !checker.isImplementationOfOverload(functionDeclaration);
}

function hasNameOrDefault(functionOrClassDeclaration: FunctionDeclaration | ClassDeclaration): boolean {
    if (!functionOrClassDeclaration.name) {
        const defaultKeyword = findModifier(functionOrClassDeclaration, SyntaxKind.DefaultKeyword);
        return !!defaultKeyword;
    }
    return true;
}

function isValidParameterNodeArray(
    parameters: NodeArray<ParameterDeclaration>,
    checker: TypeChecker): parameters is ValidParameterNodeArray {
    return getRefactorableParametersLength(parameters) >= minimumParameterLength
        && every(parameters, /*callback*/ paramDecl => isValidParameterDeclaration(paramDecl, checker));
}

function isValidParameterDeclaration(
    parameterDeclaration: ParameterDeclaration,
    checker: TypeChecker): parameterDeclaration is ValidParameterDeclaration {
    if (isRestParameter(parameterDeclaration)) {
        const type = checker.getTypeAtLocation(parameterDeclaration);
        if (!checker.isArrayType(type) && !checker.isTupleType(type)) return false;
    }
    return !parameterDeclaration.modifiers && isIdentifier(parameterDeclaration.name);
}

function isValidVariableDeclaration(node: Node): node is ValidVariableDeclaration {
    return isVariableDeclaration(node) && isVarConst(node) && isIdentifier(node.name) && !node.type; // TODO: GH#30113
}

function hasThisParameter(parameters: NodeArray<ParameterDeclaration>): boolean {
    return parameters.length > 0 && isThis(parameters[0].name);
}

function getRefactorableParametersLength(parameters: NodeArray<ParameterDeclaration>): number {
    if (hasThisParameter(parameters)) {
        return parameters.length - 1;
    }
    return parameters.length;
}

function getRefactorableParameters(parameters: NodeArray<ValidParameterDeclaration>): NodeArray<ValidParameterDeclaration> {
    if (hasThisParameter(parameters)) {
        parameters = factory.createNodeArray(parameters.slice(1), parameters.hasTrailingComma);
    }
    return parameters;
}

function createPropertyOrShorthandAssignment(name: string, initializer: Expression): PropertyAssignment | ShorthandPropertyAssignment {
    if (isIdentifier(initializer) && getTextOfIdentifierOrLiteral(initializer) === name) {
        return factory.createShorthandPropertyAssignment(name);
    }
    return factory.createPropertyAssignment(name, initializer);
}

/**
 * Creates a new object literal expression based on the provided function declaration and arguments.
 * @param functionDeclaration - The function declaration to use as a template for the object literal.
 * @param functionArguments - The arguments to use for the object literal properties.
 * @returns An object literal expression.
 */
function createNewArgument(functionDeclaration: ValidFunctionDeclaration, functionArguments: NodeArray<Expression>): ObjectLiteralExpression {
    const parameters = getRefactorableParameters(functionDeclaration.parameters);
    const hasRestParameter = isRestParameter(last(parameters));
    const nonRestArguments = hasRestParameter ? functionArguments.slice(0, parameters.length - 1) : functionArguments;
    const properties = map(nonRestArguments, (arg, i) => {
        const parameterName = getParameterName(parameters[i]);
        const property = createPropertyOrShorthandAssignment(parameterName, arg);

        suppressLeadingAndTrailingTrivia(property.name);
        if (isPropertyAssignment(property)) suppressLeadingAndTrailingTrivia(property.initializer);
        copyComments(arg, property);
        return property;
    });

    if (hasRestParameter && functionArguments.length >= parameters.length) {
        const restArguments = functionArguments.slice(parameters.length - 1);
        const restProperty = factory.createPropertyAssignment(getParameterName(last(parameters)), factory.createArrayLiteralExpression(restArguments));
        properties.push(restProperty);
    }

    const objectLiteral = factory.createObjectLiteralExpression(properties, /*multiLine*/ false);
    return objectLiteral;
}

/**
 * Creates a new set of parameters for a given function declaration or method signature.
 * @param functionDeclaration The function declaration or method signature to create new parameters for.
 * @param program The TypeScript program.
 * @param host The language service host.
 * @returns A NodeArray of ParameterDeclarations representing the new set of parameters.
 */
function createNewParameters(functionDeclaration: ValidFunctionDeclaration | ValidMethodSignature, program: Program, host: LanguageServiceHost): NodeArray<ParameterDeclaration> {
    const checker = program.getTypeChecker();
    const refactorableParameters = getRefactorableParameters(functionDeclaration.parameters);
    const bindingElements = map(refactorableParameters, createBindingElementFromParameterDeclaration);
    const objectParameterName = factory.createObjectBindingPattern(bindingElements);
    const objectParameterType = createParameterTypeNode(refactorableParameters);

    let objectInitializer: Expression | undefined;
    // If every parameter in the original function was optional, add an empty object initializer to the new object parameter
    if (every(refactorableParameters, isOptionalParameter)) {
        objectInitializer = factory.createObjectLiteralExpression();
    }

    const objectParameter = factory.createParameterDeclaration(
        /*modifiers*/ undefined,
        /*dotDotDotToken*/ undefined,
        objectParameterName,
        /*questionToken*/ undefined,
        objectParameterType,
        objectInitializer);

    if (hasThisParameter(functionDeclaration.parameters)) {
        const thisParameter = functionDeclaration.parameters[0];
        const newThisParameter = factory.createParameterDeclaration(
            /*modifiers*/ undefined,
            /*dotDotDotToken*/ undefined,
            thisParameter.name,
            /*questionToken*/ undefined,
            thisParameter.type);

        suppressLeadingAndTrailingTrivia(newThisParameter.name);
        copyComments(thisParameter.name, newThisParameter.name);
        if (thisParameter.type) {
            suppressLeadingAndTrailingTrivia(newThisParameter.type!);
            copyComments(thisParameter.type, newThisParameter.type!);
        }

        return factory.createNodeArray([newThisParameter, objectParameter]);
    }
    return factory.createNodeArray([objectParameter]);

    /**
     * Creates a binding element from a valid parameter declaration.
     * @param {ValidParameterDeclaration} parameterDeclaration - The parameter declaration to create the binding element from.
     * @returns {BindingElement} The created binding element.
     */
    function createBindingElementFromParameterDeclaration(parameterDeclaration: ValidParameterDeclaration): BindingElement {
        const element = factory.createBindingElement(
            /*dotDotDotToken*/ undefined,
            /*propertyName*/ undefined,
            getParameterName(parameterDeclaration),
            isRestParameter(parameterDeclaration) && isOptionalParameter(parameterDeclaration) ? factory.createArrayLiteralExpression() : parameterDeclaration.initializer);

        suppressLeadingAndTrailingTrivia(element);
        if (parameterDeclaration.initializer && element.initializer) {
            copyComments(parameterDeclaration.initializer, element.initializer);
        }
        return element;
    }

    function createParameterTypeNode(parameters: NodeArray<ValidParameterDeclaration>): TypeLiteralNode {
        const members = map(parameters, createPropertySignatureFromParameterDeclaration);
        const typeNode = addEmitFlags(factory.createTypeLiteralNode(members), EmitFlags.SingleLine);
        return typeNode;
    }

    /**
     * Creates a PropertySignature from a ValidParameterDeclaration object.
     * @param {ValidParameterDeclaration} parameterDeclaration - The parameter declaration to create the PropertySignature from.
     * @returns {PropertySignature} - The created PropertySignature.
     */
    function createPropertySignatureFromParameterDeclaration(parameterDeclaration: ValidParameterDeclaration): PropertySignature {
        let parameterType = parameterDeclaration.type;
        if (!parameterType && (parameterDeclaration.initializer || isRestParameter(parameterDeclaration))) {
            parameterType = getTypeNode(parameterDeclaration);
        }

        const propertySignature = factory.createPropertySignature(
            /*modifiers*/ undefined,
            getParameterName(parameterDeclaration),
            isOptionalParameter(parameterDeclaration) ? factory.createToken(SyntaxKind.QuestionToken) : parameterDeclaration.questionToken,
            parameterType);

        suppressLeadingAndTrailingTrivia(propertySignature);
        copyComments(parameterDeclaration.name, propertySignature.name);
        if (parameterDeclaration.type && propertySignature.type) {
            copyComments(parameterDeclaration.type, propertySignature.type);
        }

        return propertySignature;
    }

    function getTypeNode(node: Node): TypeNode | undefined {
        const type = checker.getTypeAtLocation(node);
        return getTypeNodeIfAccessible(type, node, program, host);
    }

    function isOptionalParameter(parameterDeclaration: ValidParameterDeclaration): boolean {
        if (isRestParameter(parameterDeclaration)) {
            const type = checker.getTypeAtLocation(parameterDeclaration);
            return !checker.isTupleType(type);
        }
        return checker.isOptionalParameter(parameterDeclaration);
    }
}

function getParameterName(paramDeclaration: ValidParameterDeclaration) {
    return getTextOfIdentifierOrLiteral(paramDeclaration.name);
}

/**
 * Returns an array of Identifier or Modifier objects representing the class names of a given constructor declaration.
 * @param constructorDeclaration - A ValidConstructor object representing the constructor declaration.
 * @returns An array of Identifier or Modifier objects representing the class names.
 */
function getClassNames(constructorDeclaration: ValidConstructor): (Identifier | Modifier)[] {
    switch (constructorDeclaration.parent.kind) {
        case SyntaxKind.ClassDeclaration:
            const classDeclaration = constructorDeclaration.parent;
            if (classDeclaration.name) return [classDeclaration.name];
            // If the class declaration doesn't have a name, it should have a default modifier.
            // We validated this in `isValidFunctionDeclaration` through `hasNameOrDefault`
            const defaultModifier = Debug.checkDefined(
                findModifier(classDeclaration, SyntaxKind.DefaultKeyword),
                "Nameless class declaration should be a default export");
            return [defaultModifier];
        case SyntaxKind.ClassExpression:
            const classExpression = constructorDeclaration.parent;
            const variableDeclaration = constructorDeclaration.parent.parent;
            const className = classExpression.name;
            if (className) return [className, variableDeclaration.name];
            return [variableDeclaration.name];
    }
}

/**
 * Returns an array of Node objects representing the names of the functions in the provided ValidFunctionDeclaration object.
 * @param {ValidFunctionDeclaration} functionDeclaration - The function declaration object to extract function names from.
 * @returns {Node[]} An array of Node objects representing the names of the functions in the provided ValidFunctionDeclaration object.
 */
function getFunctionNames(functionDeclaration: ValidFunctionDeclaration): Node[] {
    switch (functionDeclaration.kind) {
        case SyntaxKind.FunctionDeclaration:
            if (functionDeclaration.name) return [functionDeclaration.name];
            // If the function declaration doesn't have a name, it should have a default modifier.
            // We validated this in `isValidFunctionDeclaration` through `hasNameOrDefault`
            const defaultModifier = Debug.checkDefined(
                findModifier(functionDeclaration, SyntaxKind.DefaultKeyword),
                "Nameless function declaration should be a default export");
            return [defaultModifier];
        case SyntaxKind.MethodDeclaration:
            return [functionDeclaration.name];
        case SyntaxKind.Constructor:
            const ctrKeyword = Debug.checkDefined(
                findChildOfKind(functionDeclaration, SyntaxKind.ConstructorKeyword, functionDeclaration.getSourceFile()),
                "Constructor declaration should have constructor keyword");
            if (functionDeclaration.parent.kind === SyntaxKind.ClassExpression) {
                const variableDeclaration = functionDeclaration.parent.parent;
                return [variableDeclaration.name, ctrKeyword];
            }
            return [ctrKeyword];
        case SyntaxKind.ArrowFunction:
            return [functionDeclaration.parent.name];
        case SyntaxKind.FunctionExpression:
            if (functionDeclaration.name) return [functionDeclaration.name, functionDeclaration.parent.name];
            return [functionDeclaration.parent.name];
        default:
            return Debug.assertNever(functionDeclaration, `Unexpected function declaration kind ${(functionDeclaration as ValidFunctionDeclaration).kind}`);
    }
}

type ValidParameterNodeArray = NodeArray<ValidParameterDeclaration>;

interface ValidVariableDeclaration extends VariableDeclaration {
    name: Identifier;
    type: undefined;
}

interface ValidConstructor extends ConstructorDeclaration {
    parent: ClassDeclaration | (ClassExpression & { parent: ValidVariableDeclaration });
    parameters: NodeArray<ValidParameterDeclaration>;
    body: FunctionBody;
}

interface ValidFunction extends FunctionDeclaration {
    parameters: NodeArray<ValidParameterDeclaration>;
    body: FunctionBody;
}

interface ValidMethod extends MethodDeclaration {
    parameters: NodeArray<ValidParameterDeclaration>;
    body: FunctionBody;
}

interface ValidFunctionExpression extends FunctionExpression {
    parent: ValidVariableDeclaration;
    parameters: NodeArray<ValidParameterDeclaration>;
}

interface ValidArrowFunction extends ArrowFunction {
    parent: ValidVariableDeclaration;
    parameters: NodeArray<ValidParameterDeclaration>;
}

interface ValidMethodSignature extends MethodSignature {
    parameters: NodeArray<ValidParameterDeclaration>;
}

type ValidFunctionDeclaration = ValidConstructor | ValidFunction | ValidMethod | ValidArrowFunction | ValidFunctionExpression;

interface ValidParameterDeclaration extends ParameterDeclaration {
    name: Identifier;
    modifiers: undefined;
    illegalDecorators: undefined;
}

interface GroupedReferences {
    functionCalls: (CallExpression | NewExpression)[];
    declarations: Node[];
    signature?: ValidMethodSignature;
    classReferences?: ClassReferences;
    valid: boolean;
}
interface ClassReferences {
    accessExpressions: Node[];
    typeUsages: Node[];
}
