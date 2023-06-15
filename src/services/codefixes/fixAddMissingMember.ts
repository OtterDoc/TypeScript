import {
    __String,
    addToSeen,
    arrayFrom,
    BigIntLiteralType,
    BinaryExpression,
    CallExpression,
    CheckFlags,
    ClassLikeDeclaration,
    CodeFixAction,
    CodeFixContext,
    CodeFixContextBase,
    concatenate,
    createPropertyNameNodeForIdentifierOrLiteral,
    Debug,
    Diagnostics,
    emptyArray,
    EnumDeclaration,
    Expression,
    factory,
    filter,
    find,
    findAncestor,
    findIndex,
    firstDefined,
    firstOrUndefined,
    firstOrUndefinedIterator,
    FunctionExpression,
    getCheckFlags,
    getClassLikeDeclarationOfSymbol,
    getEmitScriptTarget,
    getEscapedTextOfJsxAttributeName,
    getFirstConstructorWithBody,
    getNodeId,
    getObjectFlags,
    getOrUpdate,
    getQuotePreference,
    getSourceFileOfNode,
    getTokenAtPosition,
    hasAbstractModifier,
    hasInitializer,
    Identifier,
    idText,
    InterfaceDeclaration,
    isCallExpression,
    isClassLike,
    isComputedPropertyName,
    isConstructorDeclaration,
    isEnumDeclaration,
    isFunctionTypeNode,
    isIdentifier,
    isIdentifierText,
    isInterfaceDeclaration,
    isJsxAttribute,
    isJsxExpression,
    isJsxOpeningLikeElement,
    isJsxSpreadAttribute,
    isMemberName,
    isMethodDeclaration,
    isMethodSignature,
    isModuleDeclaration,
    isObjectLiteralExpression,
    isParameter,
    isPrivateIdentifier,
    isPropertyAccessExpression,
    isPropertyDeclaration,
    isReturnStatement,
    isSourceFile,
    isSourceFileFromLibrary,
    isSourceFileJS,
    isTransientSymbol,
    isTypeLiteralNode,
    JsxOpeningLikeElement,
    LanguageVariant,
    length,
    map,
    MethodDeclaration,
    ModifierFlags,
    ModuleDeclaration,
    Node,
    NodeBuilderFlags,
    NumberLiteralType,
    ObjectFlags,
    ObjectLiteralExpression,
    or,
    PrivateIdentifier,
    Program,
    PropertyDeclaration,
    QuotePreference,
    ReturnStatement,
    ScriptTarget,
    setParent,
    Signature,
    SignatureKind,
    singleElementArray,
    singleOrUndefined,
    skipConstraint,
    some,
    SourceFile,
    startsWithUnderscore,
    StringLiteralType,
    Symbol,
    SymbolFlags,
    SyntaxKind,
    textChanges,
    tryCast,
    Type,
    TypeChecker,
    TypeFlags,
    TypeLiteralNode,
    TypeNode,
    TypeReference,
    UnionType,
} from "../_namespaces/ts";
import {
    createCodeFixAction,
    createCodeFixActionWithoutFixAll,
    createCombinedCodeActions,
    createImportAdder,
    createSignatureDeclarationFromCallExpression,
    createSignatureDeclarationFromSignature,
    createStubbedBody,
    eachDiagnostic,
    getAllSupers,
    ImportAdder,
    registerCodeFix,
} from "../_namespaces/ts.codefix";

const fixMissingMember = "fixMissingMember";
const fixMissingProperties = "fixMissingProperties";
const fixMissingAttributes = "fixMissingAttributes";
const fixMissingFunctionDeclaration = "fixMissingFunctionDeclaration";

const errorCodes = [
    Diagnostics.Property_0_does_not_exist_on_type_1.code,
    Diagnostics.Property_0_does_not_exist_on_type_1_Did_you_mean_2.code,
    Diagnostics.Property_0_is_missing_in_type_1_but_required_in_type_2.code,
    Diagnostics.Type_0_is_missing_the_following_properties_from_type_1_Colon_2.code,
    Diagnostics.Type_0_is_missing_the_following_properties_from_type_1_Colon_2_and_3_more.code,
    Diagnostics.Argument_of_type_0_is_not_assignable_to_parameter_of_type_1.code,
    Diagnostics.Cannot_find_name_0.code
];

enum InfoKind {
    TypeLikeDeclaration,
    Enum,
    Function,
    ObjectLiteral,
    JsxAttributes,
    Signature,
}

registerCodeFix({
    errorCodes,
    getCodeActions(context) {
        const typeChecker = context.program.getTypeChecker();
        const info = getInfo(context.sourceFile, context.span.start, context.errorCode, typeChecker, context.program);
        if (!info) {
            return undefined;
        }
        if (info.kind === InfoKind.ObjectLiteral) {
            const changes = textChanges.ChangeTracker.with(context, t => addObjectLiteralProperties(t, context, info));
            return [createCodeFixAction(fixMissingProperties, changes, Diagnostics.Add_missing_properties, fixMissingProperties, Diagnostics.Add_all_missing_properties)];
        }
        if (info.kind === InfoKind.JsxAttributes) {
            const changes = textChanges.ChangeTracker.with(context, t => addJsxAttributes(t, context, info));
            return [createCodeFixAction(fixMissingAttributes, changes, Diagnostics.Add_missing_attributes, fixMissingAttributes, Diagnostics.Add_all_missing_attributes)];
        }
        if (info.kind === InfoKind.Function || info.kind === InfoKind.Signature) {
            const changes = textChanges.ChangeTracker.with(context, t => addFunctionDeclaration(t, context, info));
            return [createCodeFixAction(fixMissingFunctionDeclaration, changes, [Diagnostics.Add_missing_function_declaration_0, info.token.text], fixMissingFunctionDeclaration, Diagnostics.Add_all_missing_function_declarations)];
        }
        if (info.kind === InfoKind.Enum) {
            const changes = textChanges.ChangeTracker.with(context, t => addEnumMemberDeclaration(t, context.program.getTypeChecker(), info));
            return [createCodeFixAction(fixMissingMember, changes, [Diagnostics.Add_missing_enum_member_0, info.token.text], fixMissingMember, Diagnostics.Add_all_missing_members)];
        }
        return concatenate(getActionsForMissingMethodDeclaration(context, info), getActionsForMissingMemberDeclaration(context, info));
    },
    fixIds: [fixMissingMember, fixMissingFunctionDeclaration, fixMissingProperties, fixMissingAttributes],
    getAllCodeActions: context => {
        const { program, fixId } = context;
        const checker = program.getTypeChecker();
        const seen = new Map<string, true>();
        const typeDeclToMembers = new Map<ClassLikeDeclaration | InterfaceDeclaration | TypeLiteralNode, TypeLikeDeclarationInfo[]>();

        return createCombinedCodeActions(textChanges.ChangeTracker.with(context, changes => {
            eachDiagnostic(context, errorCodes, diag => {
                const info = getInfo(diag.file, diag.start, diag.code, checker, context.program);
                if (!info || !addToSeen(seen, getNodeId(info.parentDeclaration) + "#" + info.token.text)) {
                    return;
                }
                if (fixId === fixMissingFunctionDeclaration && (info.kind === InfoKind.Function || info.kind === InfoKind.Signature)) {
                    addFunctionDeclaration(changes, context, info);
                }
                else if (fixId === fixMissingProperties && info.kind === InfoKind.ObjectLiteral) {
                    addObjectLiteralProperties(changes, context, info);
                }
                else if (fixId === fixMissingAttributes && info.kind === InfoKind.JsxAttributes) {
                    addJsxAttributes(changes, context, info);
                }
                else {
                    if (info.kind === InfoKind.Enum) {
                        addEnumMemberDeclaration(changes, checker, info);
                    }
                    if (info.kind === InfoKind.TypeLikeDeclaration) {
                        const { parentDeclaration, token } = info;
                        const infos = getOrUpdate(typeDeclToMembers, parentDeclaration, () => []);
                        if (!infos.some(i => i.token.text === token.text)) {
                            infos.push(info);
                        }
                    }
                }
            });

            typeDeclToMembers.forEach((infos, declaration) => {
                const supers = isTypeLiteralNode(declaration) ? undefined : getAllSupers(declaration, checker);
                for (const info of infos) {
                    // If some superclass added this property, don't add it again.
                    if (supers?.some(superClassOrInterface => {
                        const superInfos = typeDeclToMembers.get(superClassOrInterface);
                        return !!superInfos && superInfos.some(({ token }) => token.text === info.token.text);
                    })) continue;

                    const { parentDeclaration, declSourceFile, modifierFlags, token, call, isJSFile } = info;
                    // Always prefer to add a method declaration if possible.
                    if (call && !isPrivateIdentifier(token)) {
                        addMethodDeclaration(context, changes, call, token, modifierFlags & ModifierFlags.Static, parentDeclaration, declSourceFile);
                    }
                    else {
                        if (isJSFile && !isInterfaceDeclaration(parentDeclaration) && !isTypeLiteralNode(parentDeclaration)) {
                            addMissingMemberInJs(changes, declSourceFile, parentDeclaration, token, !!(modifierFlags & ModifierFlags.Static));
                        }
                        else {
                            const typeNode = getTypeNode(checker, parentDeclaration, token);
                            addPropertyDeclaration(changes, declSourceFile, parentDeclaration, token.text, typeNode, modifierFlags & ModifierFlags.Static);
                        }
                    }
                }
            });
        }));
    },
});

type Info = TypeLikeDeclarationInfo | EnumInfo | FunctionInfo | ObjectLiteralInfo | JsxAttributesInfo | SignatureInfo;

interface EnumInfo {
    readonly kind: InfoKind.Enum;
    readonly token: Identifier;
    readonly parentDeclaration: EnumDeclaration;
}

interface TypeLikeDeclarationInfo {
    readonly kind: InfoKind.TypeLikeDeclaration;
    readonly call: CallExpression | undefined;
    readonly token: Identifier | PrivateIdentifier;
    readonly modifierFlags: ModifierFlags;
    readonly parentDeclaration: ClassLikeDeclaration | InterfaceDeclaration | TypeLiteralNode;
    readonly declSourceFile: SourceFile;
    readonly isJSFile: boolean;
}

interface FunctionInfo {
    readonly kind: InfoKind.Function;
    readonly call: CallExpression;
    readonly token: Identifier;
    readonly sourceFile: SourceFile;
    readonly modifierFlags: ModifierFlags;
    readonly parentDeclaration: SourceFile | ModuleDeclaration | ReturnStatement;
}

interface ObjectLiteralInfo {
    readonly kind: InfoKind.ObjectLiteral;
    readonly token: Identifier;
    readonly properties: Symbol[];
    readonly parentDeclaration: ObjectLiteralExpression;
    readonly indentation?: number;
}

interface JsxAttributesInfo {
    readonly kind: InfoKind.JsxAttributes;
    readonly token: Identifier;
    readonly attributes: Symbol[];
    readonly parentDeclaration: JsxOpeningLikeElement;
}

interface SignatureInfo {
    readonly kind: InfoKind.Signature;
    readonly token: Identifier;
    readonly signature: Signature;
    readonly sourceFile: SourceFile;
    readonly parentDeclaration: Node;
}

function getInfo(sourceFile: SourceFile, tokenPos: number, errorCode: number, checker: TypeChecker, program: Program): Info | undefined {
    // The identifier of the missing property. eg:
    // this.missing = 1;
    //      ^^^^^^^
    const token = getTokenAtPosition(sourceFile, tokenPos);
    const parent = token.parent;

    if (errorCode === Diagnostics.Argument_of_type_0_is_not_assignable_to_parameter_of_type_1.code) {
        if (!(token.kind === SyntaxKind.OpenBraceToken && isObjectLiteralExpression(parent) && isCallExpression(parent.parent))) return undefined;

        const argIndex = findIndex(parent.parent.arguments, arg => arg === parent);
        if (argIndex < 0) return undefined;

        const signature = checker.getResolvedSignature(parent.parent);
        if (!(signature && signature.declaration && signature.parameters[argIndex])) return undefined;

        const param = signature.parameters[argIndex].valueDeclaration;
        if (!(param && isParameter(param) && isIdentifier(param.name))) return undefined;

        const properties = arrayFrom(checker.getUnmatchedProperties(checker.getTypeAtLocation(parent), checker.getParameterType(signature, argIndex), /*requireOptionalProperties*/ false, /*matchDiscriminantProperties*/ false));
        if (!length(properties)) return undefined;
        return { kind: InfoKind.ObjectLiteral, token: param.name, properties, parentDeclaration: parent };
    }

    if (!isMemberName(token)) return undefined;

    if (isIdentifier(token) && hasInitializer(parent) && parent.initializer && isObjectLiteralExpression(parent.initializer)) {
        const targetType = checker.getContextualType(token) || checker.getTypeAtLocation(token);
        const properties = arrayFrom(checker.getUnmatchedProperties(checker.getTypeAtLocation(parent.initializer), targetType, /*requireOptionalProperties*/ false, /*matchDiscriminantProperties*/ false));
        if (!length(properties)) return undefined;

        return { kind: InfoKind.ObjectLiteral, token, properties, parentDeclaration: parent.initializer };
    }

    if (isIdentifier(token) && isJsxOpeningLikeElement(token.parent)) {
        const target = getEmitScriptTarget(program.getCompilerOptions());
        const attributes = getUnmatchedAttributes(checker, target, token.parent);
        if (!length(attributes)) return undefined;
        return { kind: InfoKind.JsxAttributes, token, attributes, parentDeclaration: token.parent };
    }

    if (isIdentifier(token)) {
        const type = checker.getContextualType(token)?.getNonNullableType();
        if (type && getObjectFlags(type) & ObjectFlags.Anonymous) {
            const signature = firstOrUndefined(checker.getSignaturesOfType(type, SignatureKind.Call));
            if (signature === undefined) return undefined;
            return { kind: InfoKind.Signature, token, signature, sourceFile, parentDeclaration: findScope(token) };
        }
        if (isCallExpression(parent) && parent.expression === token) {
            return { kind: InfoKind.Function, token, call: parent, sourceFile, modifierFlags: ModifierFlags.None, parentDeclaration: findScope(token) };
        }
    }

    if (!isPropertyAccessExpression(parent)) return undefined;

    const leftExpressionType = skipConstraint(checker.getTypeAtLocation(parent.expression));
    const symbol = leftExpressionType.symbol;
    if (!symbol || !symbol.declarations) return undefined;

    if (isIdentifier(token) && isCallExpression(parent.parent)) {
        const moduleDeclaration = find(symbol.declarations, isModuleDeclaration);
        const moduleDeclarationSourceFile = moduleDeclaration?.getSourceFile();
        if (moduleDeclaration && moduleDeclarationSourceFile && !isSourceFileFromLibrary(program, moduleDeclarationSourceFile)) {
            return { kind: InfoKind.Function, token, call: parent.parent, sourceFile, modifierFlags: ModifierFlags.Export, parentDeclaration: moduleDeclaration };
        }

        const moduleSourceFile = find(symbol.declarations, isSourceFile);
        if (sourceFile.commonJsModuleIndicator) return undefined;

        if (moduleSourceFile && !isSourceFileFromLibrary(program, moduleSourceFile)) {
            return { kind: InfoKind.Function, token, call: parent.parent, sourceFile: moduleSourceFile, modifierFlags: ModifierFlags.Export, parentDeclaration: moduleSourceFile };
        }
    }

    const classDeclaration = find(symbol.declarations, isClassLike);
    // Don't suggest adding private identifiers to anything other than a class.
    if (!classDeclaration && isPrivateIdentifier(token)) return undefined;

    // Prefer to change the class instead of the interface if they are merged
    const declaration = classDeclaration || find(symbol.declarations, d => isInterfaceDeclaration(d) || isTypeLiteralNode(d)) as InterfaceDeclaration | TypeLiteralNode | undefined;
    if (declaration && !isSourceFileFromLibrary(program, declaration.getSourceFile())) {
        const makeStatic = !isTypeLiteralNode(declaration) && ((leftExpressionType as TypeReference).target || leftExpressionType) !== checker.getDeclaredTypeOfSymbol(symbol);
        if (makeStatic && (isPrivateIdentifier(token) || isInterfaceDeclaration(declaration))) return undefined;

        const declSourceFile = declaration.getSourceFile();
        const modifierFlags = isTypeLiteralNode(declaration) ? ModifierFlags.None :
            (makeStatic ? ModifierFlags.Static : ModifierFlags.None) | (startsWithUnderscore(token.text) ? ModifierFlags.Private : ModifierFlags.None);
        const isJSFile = isSourceFileJS(declSourceFile);
        const call = tryCast(parent.parent, isCallExpression);
        return { kind: InfoKind.TypeLikeDeclaration, token, call, modifierFlags, parentDeclaration: declaration, declSourceFile, isJSFile };
    }

    const enumDeclaration = find(symbol.declarations, isEnumDeclaration);
    if (enumDeclaration && !(leftExpressionType.flags & TypeFlags.EnumLike) && !isPrivateIdentifier(token) && !isSourceFileFromLibrary(program, enumDeclaration.getSourceFile())) {
        return { kind: InfoKind.Enum, token, parentDeclaration: enumDeclaration };
    }

    return undefined;
}

function getActionsForMissingMemberDeclaration(context: CodeFixContext, info: TypeLikeDeclarationInfo): CodeFixAction[] | undefined {
    return info.isJSFile ? singleElementArray(createActionForAddMissingMemberInJavascriptFile(context, info)) :
        createActionsForAddMissingMemberInTypeScriptFile(context, info);
}

/**
 * Creates a code fix action for adding a missing member in a JavaScript file.
 * @param context - The code fix context.
 * @param TypeLikeDeclarationInfo - An object containing information about the type declaration.
 * @param TypeLikeDeclarationInfo.parentDeclaration - The parent declaration.
 * @param TypeLikeDeclarationInfo.declSourceFile - The declaration source file.
 * @param TypeLikeDeclarationInfo.modifierFlags - The modifier flags.
 * @param TypeLikeDeclarationInfo.token - The token.
 * @returns A code fix action or undefined.
 */
function createActionForAddMissingMemberInJavascriptFile(context: CodeFixContext, { parentDeclaration, declSourceFile, modifierFlags, token }: TypeLikeDeclarationInfo): CodeFixAction | undefined {
    if (isInterfaceDeclaration(parentDeclaration) || isTypeLiteralNode(parentDeclaration)) {
        return undefined;
    }

    const changes = textChanges.ChangeTracker.with(context, t => addMissingMemberInJs(t, declSourceFile, parentDeclaration, token, !!(modifierFlags & ModifierFlags.Static)));
    if (changes.length === 0) {
        return undefined;
    }

    const diagnostic = modifierFlags & ModifierFlags.Static ? Diagnostics.Initialize_static_property_0 :
        isPrivateIdentifier(token) ? Diagnostics.Declare_a_private_field_named_0 : Diagnostics.Initialize_property_0_in_the_constructor;

    return createCodeFixAction(fixMissingMember, changes, [diagnostic, token.text], fixMissingMember, Diagnostics.Add_all_missing_members);
}

/**
 * Adds a missing member to a class declaration in a TypeScript file.
 * @param changeTracker - The text changes tracker.
 * @param sourceFile - The source file containing the class declaration.
 * @param classDeclaration - The class declaration to add the member to.
 * @param token - The identifier or private identifier of the member to add.
 * @param makeStatic - Whether the member should be static or not.
 * @returns void.
 */
function addMissingMemberInJs(changeTracker: textChanges.ChangeTracker, sourceFile: SourceFile, classDeclaration: ClassLikeDeclaration, token: Identifier | PrivateIdentifier, makeStatic: boolean): void {
    const tokenName = token.text;
    if (makeStatic) {
        if (classDeclaration.kind === SyntaxKind.ClassExpression) {
            return;
        }
        const className = classDeclaration.name!.getText();
        const staticInitialization = initializePropertyToUndefined(factory.createIdentifier(className), tokenName);
        changeTracker.insertNodeAfter(sourceFile, classDeclaration, staticInitialization);
    }
    else if (isPrivateIdentifier(token)) {
        const property = factory.createPropertyDeclaration(
            /*modifiers*/ undefined,
            tokenName,
            /*questionOrExclamationToken*/ undefined,
            /*type*/ undefined,
            /*initializer*/ undefined);

        const lastProp = getNodeToInsertPropertyAfter(classDeclaration);
        if (lastProp) {
            changeTracker.insertNodeAfter(sourceFile, lastProp, property);
        }
        else {
            changeTracker.insertMemberAtStart(sourceFile, classDeclaration, property);
        }
    }
    else {
        const classConstructor = getFirstConstructorWithBody(classDeclaration);
        if (!classConstructor) {
            return;
        }
        const propertyInitialization = initializePropertyToUndefined(factory.createThis(), tokenName);
        changeTracker.insertNodeAtConstructorEnd(sourceFile, classConstructor, propertyInitialization);
    }
}

function initializePropertyToUndefined(obj: Expression, propertyName: string) {
    return factory.createExpressionStatement(factory.createAssignment(factory.createPropertyAccessExpression(obj, propertyName), createUndefined()));
}

/**
 * Creates code fix actions for adding a missing member in a TypeScript file.
 * @param context The code fix context.
 * @param parentDeclaration The parent declaration.
 * @param declSourceFile The declaration source file.
 * @param modifierFlags The modifier flags.
 * @param token The token.
 * @returns An array of code fix actions or undefined.
 */
function createActionsForAddMissingMemberInTypeScriptFile(context: CodeFixContext, { parentDeclaration, declSourceFile, modifierFlags, token }: TypeLikeDeclarationInfo): CodeFixAction[] | undefined {
    const memberName = token.text;
    const isStatic = modifierFlags & ModifierFlags.Static;
    const typeNode = getTypeNode(context.program.getTypeChecker(), parentDeclaration, token);
    const addPropertyDeclarationChanges = (modifierFlags: ModifierFlags) => textChanges.ChangeTracker.with(context, t => addPropertyDeclaration(t, declSourceFile, parentDeclaration, memberName, typeNode, modifierFlags));

    const actions = [createCodeFixAction(fixMissingMember, addPropertyDeclarationChanges(modifierFlags & ModifierFlags.Static), [isStatic ? Diagnostics.Declare_static_property_0 : Diagnostics.Declare_property_0, memberName], fixMissingMember, Diagnostics.Add_all_missing_members)];
    if (isStatic || isPrivateIdentifier(token)) {
        return actions;
    }

    if (modifierFlags & ModifierFlags.Private) {
        actions.unshift(createCodeFixActionWithoutFixAll(fixMissingMember, addPropertyDeclarationChanges(ModifierFlags.Private), [Diagnostics.Declare_private_property_0, memberName]));
    }

    actions.push(createAddIndexSignatureAction(context, declSourceFile, parentDeclaration, token.text, typeNode));
    return actions;
}

/**
 * Returns a TypeNode based on the given TypeChecker, node, and token.
 * @param {TypeChecker} checker - The TypeChecker object.
 * @param {ClassLikeDeclaration | InterfaceDeclaration | TypeLiteralNode} node - The node to get the TypeNode from.
 * @param {Node} token - The token to use for contextual typing.
 * @returns {TypeNode} - The resulting TypeNode.
 */
function getTypeNode(checker: TypeChecker, node: ClassLikeDeclaration | InterfaceDeclaration | TypeLiteralNode, token: Node) {
    let typeNode: TypeNode | undefined;
    if (token.parent.parent.kind === SyntaxKind.BinaryExpression) {
        const binaryExpression = token.parent.parent as BinaryExpression;
        const otherExpression = token.parent === binaryExpression.left ? binaryExpression.right : binaryExpression.left;
        const widenedType = checker.getWidenedType(checker.getBaseTypeOfLiteralType(checker.getTypeAtLocation(otherExpression)));
        typeNode = checker.typeToTypeNode(widenedType, node, NodeBuilderFlags.NoTruncation);
    }
    else {
        const contextualType = checker.getContextualType(token.parent as Expression);
        typeNode = contextualType ? checker.typeToTypeNode(contextualType, /*enclosingDeclaration*/ undefined, NodeBuilderFlags.NoTruncation) : undefined;
    }
    return typeNode || factory.createKeywordTypeNode(SyntaxKind.AnyKeyword);
}

/**
 * Adds a property declaration to a class, interface, or type literal node.
 * @param changeTracker - The text changes change tracker.
 * @param sourceFile - The source file containing the node.
 * @param node - The class, interface, or type literal node to add the property to.
 * @param tokenName - The name of the property.
 * @param typeNode - The type of the property.
 * @param modifierFlags - The modifier flags for the property.
 * @returns void
 */
function addPropertyDeclaration(changeTracker: textChanges.ChangeTracker, sourceFile: SourceFile, node: ClassLikeDeclaration | InterfaceDeclaration | TypeLiteralNode, tokenName: string, typeNode: TypeNode, modifierFlags: ModifierFlags): void {
    const modifiers = modifierFlags ? factory.createNodeArray(factory.createModifiersFromModifierFlags(modifierFlags)) : undefined;

    const property = isClassLike(node)
        ? factory.createPropertyDeclaration(modifiers, tokenName, /*questionOrExclamationToken*/ undefined, typeNode, /*initializer*/ undefined)
        : factory.createPropertySignature(/*modifiers*/ undefined, tokenName, /*questionToken*/ undefined, typeNode);

    const lastProp = getNodeToInsertPropertyAfter(node);
    if (lastProp) {
        changeTracker.insertNodeAfter(sourceFile, lastProp, property);
    }
    else {
        changeTracker.insertMemberAtStart(sourceFile, node, property);
    }
}

// Gets the last of the first run of PropertyDeclarations, or undefined if the class does not start with a PropertyDeclaration.
function getNodeToInsertPropertyAfter(node: ClassLikeDeclaration | InterfaceDeclaration | TypeLiteralNode): PropertyDeclaration | undefined {
    let res: PropertyDeclaration | undefined;
    for (const member of node.members) {
        if (!isPropertyDeclaration(member)) break;
        res = member;
    }
    return res;
}

/**
 * Creates a code fix action to add an index signature for a property.
 * @param context - The code fix context.
 * @param sourceFile - The source file.
 * @param node - The class, interface, or type literal node.
 * @param tokenName - The name of the token.
 * @param typeNode - The type node.
 * @returns The code fix action.
 */
function createAddIndexSignatureAction(context: CodeFixContext, sourceFile: SourceFile, node: ClassLikeDeclaration | InterfaceDeclaration | TypeLiteralNode, tokenName: string, typeNode: TypeNode): CodeFixAction {
    // Index signatures cannot have the static modifier.
    const stringTypeNode = factory.createKeywordTypeNode(SyntaxKind.StringKeyword);
    const indexingParameter = factory.createParameterDeclaration(
        /*modifiers*/ undefined,
        /*dotDotDotToken*/ undefined,
        "x",
        /*questionToken*/ undefined,
        stringTypeNode,
        /*initializer*/ undefined);
    const indexSignature = factory.createIndexSignature(
        /*modifiers*/ undefined,
        [indexingParameter],
        typeNode);

    const changes = textChanges.ChangeTracker.with(context, t => t.insertMemberAtStart(sourceFile, node, indexSignature));
    // No fixId here because code-fix-all currently only works on adding individual named properties.
    return createCodeFixActionWithoutFixAll(fixMissingMember, changes, [Diagnostics.Add_index_signature_for_property_0, tokenName]);
}

/**
 * Returns an array of CodeFixAction objects or undefined. The function takes in two parameters:
 * @param {CodeFixContext} context - The context object for the code fix.
 * @param {TypeLikeDeclarationInfo} info - The TypeLikeDeclarationInfo object containing information about the missing method declaration.
 * @returns {CodeFixAction[] | undefined} - An array of CodeFixAction objects or undefined.
 */
function getActionsForMissingMethodDeclaration(context: CodeFixContext, info: TypeLikeDeclarationInfo): CodeFixAction[] | undefined {
    const { parentDeclaration, declSourceFile, modifierFlags, token, call } = info;
    if (call === undefined) {
        return undefined;
    }

    const methodName = token.text;
    const addMethodDeclarationChanges = (modifierFlags: ModifierFlags) => textChanges.ChangeTracker.with(context, t => addMethodDeclaration(context, t, call, token, modifierFlags, parentDeclaration, declSourceFile));
    const actions = [createCodeFixAction(fixMissingMember, addMethodDeclarationChanges(modifierFlags & ModifierFlags.Static), [modifierFlags & ModifierFlags.Static ? Diagnostics.Declare_static_method_0 : Diagnostics.Declare_method_0, methodName], fixMissingMember, Diagnostics.Add_all_missing_members)];
    if (modifierFlags & ModifierFlags.Private) {
        actions.unshift(createCodeFixActionWithoutFixAll(fixMissingMember, addMethodDeclarationChanges(ModifierFlags.Private), [Diagnostics.Declare_private_method_0, methodName]));
    }
    return actions;
}

/**
 * Adds a method declaration to a class, interface, or type literal node.
 *
 * @param {CodeFixContextBase} context - The context object for the code fix.
 * @param {textChanges.ChangeTracker} changes - The change tracker for making edits to the source file.
 * @param {CallExpression} callExpression - The call expression for the method being added.
 * @param {Identifier | PrivateIdentifier} name - The name of the method being added.
 * @param {ModifierFlags} modifierFlags - The modifier flags for the method being added.
 * @param {ClassLikeDeclaration | InterfaceDeclaration | TypeLiteralNode} parentDeclaration - The parent declaration for the method being added.
 * @param {SourceFile} sourceFile - The source file for the method being added.
 *
 * @returns {void}
 */
function addMethodDeclaration(
    context: CodeFixContextBase,
    changes: textChanges.ChangeTracker,
    callExpression: CallExpression,
    name: Identifier | PrivateIdentifier,
    modifierFlags: ModifierFlags,
    parentDeclaration: ClassLikeDeclaration | InterfaceDeclaration | TypeLiteralNode,
    sourceFile: SourceFile,
): void {
    const importAdder = createImportAdder(sourceFile, context.program, context.preferences, context.host);
    const kind = isClassLike(parentDeclaration) ? SyntaxKind.MethodDeclaration : SyntaxKind.MethodSignature;
    const signatureDeclaration = createSignatureDeclarationFromCallExpression(kind, context, importAdder, callExpression, name, modifierFlags, parentDeclaration) as MethodDeclaration;
    const containingMethodDeclaration = tryGetContainingMethodDeclaration(parentDeclaration, callExpression);
    if (containingMethodDeclaration) {
        changes.insertNodeAfter(sourceFile, containingMethodDeclaration, signatureDeclaration);
    }
    else {
        changes.insertMemberAtStart(sourceFile, parentDeclaration, signatureDeclaration);
    }
    importAdder.writeFixes(changes);
}

/**
 * Adds a new enum member declaration to the parent enum declaration.
 *
 * @param changes - The text changes object.
 * @param checker - The type checker object.
 * @param enumInfo - An object containing information about the parent enum declaration, including the token and parent declaration.
 * @returns void
 *
 * @remarks
 * This function creates an initializer for a literal enum that has a string initializer. The value of the initializer is a string literal that is equal to the name of the enum member. Numeric enums or empty enums will not create an initializer.
 */
function addEnumMemberDeclaration(changes: textChanges.ChangeTracker, checker: TypeChecker, { token, parentDeclaration }: EnumInfo) {
    /**
     * create initializer only literal enum that has string initializer.
     * value of initializer is a string literal that equal to name of enum member.
     * numeric enum or empty enum will not create initializer.
     */
    const hasStringInitializer = some(parentDeclaration.members, member => {
        const type = checker.getTypeAtLocation(member);
        return !!(type && type.flags & TypeFlags.StringLike);
    });

    const enumMember = factory.createEnumMember(token, hasStringInitializer ? factory.createStringLiteral(token.text) : undefined);
    changes.replaceNode(parentDeclaration.getSourceFile(), parentDeclaration, factory.updateEnumDeclaration(
        parentDeclaration,
        parentDeclaration.modifiers,
        parentDeclaration.name,
        concatenate(parentDeclaration.members, singleElementArray(enumMember))
    ), {
        leadingTriviaOption: textChanges.LeadingTriviaOption.IncludeAll,
        trailingTriviaOption: textChanges.TrailingTriviaOption.Exclude
    });
}

/**
 * Creates a function declaration from either a call expression or a signature.
 * @param changes - The text changes to apply the fix.
 * @param context - The code fix context.
 * @param info - The function or signature information.
 */
function addFunctionDeclaration(changes: textChanges.ChangeTracker, context: CodeFixContextBase, info: FunctionInfo | SignatureInfo) {
    const quotePreference = getQuotePreference(context.sourceFile, context.preferences);
    const importAdder = createImportAdder(context.sourceFile, context.program, context.preferences, context.host);
    const functionDeclaration = info.kind === InfoKind.Function
        ? createSignatureDeclarationFromCallExpression(SyntaxKind.FunctionDeclaration, context, importAdder, info.call, idText(info.token), info.modifierFlags, info.parentDeclaration)
        : createSignatureDeclarationFromSignature(SyntaxKind.FunctionDeclaration, context, quotePreference, info.signature, createStubbedBody(Diagnostics.Function_not_implemented.message, quotePreference), info.token, /*modifiers*/ undefined, /*optional*/ undefined, /*enclosingDeclaration*/ undefined, importAdder);
    if (functionDeclaration === undefined) {
        Debug.fail("fixMissingFunctionDeclaration codefix got unexpected error.");
    }

    isReturnStatement(info.parentDeclaration)
        ? changes.insertNodeBefore(info.sourceFile, info.parentDeclaration, functionDeclaration, /*blankLineBetween*/ true)
        : changes.insertNodeAtEndOfScope(info.sourceFile, info.parentDeclaration, functionDeclaration);
    importAdder.writeFixes(changes);
}

/**
 * Adds JSX attributes to a parent declaration.
 * @param changes - The text changes tracker.
 * @param context - The code fix context.
 * @param info - The JSX attributes information.
 */
function addJsxAttributes(changes: textChanges.ChangeTracker, context: CodeFixContextBase, info: JsxAttributesInfo) {
    const importAdder = createImportAdder(context.sourceFile, context.program, context.preferences, context.host);
    const quotePreference = getQuotePreference(context.sourceFile, context.preferences);
    const checker = context.program.getTypeChecker();
    const jsxAttributesNode = info.parentDeclaration.attributes;
    const hasSpreadAttribute = some(jsxAttributesNode.properties, isJsxSpreadAttribute);
    const attrs = map(info.attributes, attr => {
        const value = tryGetValueFromType(context, checker, importAdder, quotePreference, checker.getTypeOfSymbol(attr), info.parentDeclaration);
        const name = factory.createIdentifier(attr.name);
        const jsxAttribute = factory.createJsxAttribute(name, factory.createJsxExpression(/*dotDotDotToken*/ undefined, value));
        // formattingScanner requires the Identifier to have a context for scanning attributes with "-" (data-foo).
        setParent(name, jsxAttribute);
        return jsxAttribute;
    });
    const jsxAttributes = factory.createJsxAttributes(hasSpreadAttribute ? [...attrs, ...jsxAttributesNode.properties] : [...jsxAttributesNode.properties, ...attrs]);
    const options = { prefix: jsxAttributesNode.pos === jsxAttributesNode.end ? " " : undefined };
    changes.replaceNode(context.sourceFile, jsxAttributesNode, jsxAttributes, options);
    importAdder.writeFixes(changes);
}

/**
 * Adds properties to an object literal.
 * @param changes - The text changes to be made.
 * @param context - The code fix context.
 * @param info - The object literal information.
 */
function addObjectLiteralProperties(changes: textChanges.ChangeTracker, context: CodeFixContextBase, info: ObjectLiteralInfo) {
    const importAdder = createImportAdder(context.sourceFile, context.program, context.preferences, context.host);
    const quotePreference = getQuotePreference(context.sourceFile, context.preferences);
    const target = getEmitScriptTarget(context.program.getCompilerOptions());
    const checker = context.program.getTypeChecker();
    const props = map(info.properties, prop => {
        const initializer = tryGetValueFromType(context, checker, importAdder, quotePreference, checker.getTypeOfSymbol(prop), info.parentDeclaration);
        return factory.createPropertyAssignment(createPropertyNameFromSymbol(prop, target, quotePreference, checker), initializer);
    });
    const options = {
        leadingTriviaOption: textChanges.LeadingTriviaOption.Exclude,
        trailingTriviaOption: textChanges.TrailingTriviaOption.Exclude,
        indentation: info.indentation
    };
    changes.replaceNode(context.sourceFile, info.parentDeclaration, factory.createObjectLiteralExpression([...info.parentDeclaration.properties, ...props], /*multiLine*/ true), options);
    importAdder.writeFixes(changes);
}

/**
 * Tries to get a value from a given type and returns an expression.
 * @param {CodeFixContextBase} context - The context for the code fix.
 * @param {TypeChecker} checker - The type checker.
 * @param {ImportAdder} importAdder - The import adder.
 * @param {QuotePreference} quotePreference - The quote preference.
 * @param {Type} type - The type to get the value from.
 * @param {Node | undefined} enclosingDeclaration - The enclosing declaration.
 * @returns {Expression} The expression representing the value of the given type.
 */
function tryGetValueFromType(context: CodeFixContextBase, checker: TypeChecker, importAdder: ImportAdder, quotePreference: QuotePreference, type: Type, enclosingDeclaration: Node | undefined): Expression {
    if (type.flags & TypeFlags.AnyOrUnknown) {
        return createUndefined();
    }
    if (type.flags & (TypeFlags.String | TypeFlags.TemplateLiteral)) {
        return factory.createStringLiteral("", /* isSingleQuote */ quotePreference === QuotePreference.Single);
    }
    if (type.flags & TypeFlags.Number) {
        return factory.createNumericLiteral(0);
    }
    if (type.flags & TypeFlags.BigInt) {
        return factory.createBigIntLiteral("0n");
    }
    if (type.flags & TypeFlags.Boolean) {
        return factory.createFalse();
    }
    if (type.flags & TypeFlags.EnumLike) {
        const enumMember = type.symbol.exports ? firstOrUndefinedIterator(type.symbol.exports.values()) : type.symbol;
        const name = checker.symbolToExpression(type.symbol.parent ? type.symbol.parent : type.symbol, SymbolFlags.Value, /*enclosingDeclaration*/ undefined, /*flags*/ undefined);
        return enumMember === undefined || name === undefined ? factory.createNumericLiteral(0) : factory.createPropertyAccessExpression(name, checker.symbolToString(enumMember));
    }
    if (type.flags & TypeFlags.NumberLiteral) {
        return factory.createNumericLiteral((type as NumberLiteralType).value);
    }
    if (type.flags & TypeFlags.BigIntLiteral) {
        return factory.createBigIntLiteral((type as BigIntLiteralType).value);
    }
    if (type.flags & TypeFlags.StringLiteral) {
        return factory.createStringLiteral((type as StringLiteralType).value, /* isSingleQuote */ quotePreference === QuotePreference.Single);
    }
    if (type.flags & TypeFlags.BooleanLiteral) {
        return (type === checker.getFalseType() || type === checker.getFalseType(/*fresh*/ true)) ? factory.createFalse() : factory.createTrue();
    }
    if (type.flags & TypeFlags.Null) {
        return factory.createNull();
    }
    if (type.flags & TypeFlags.Union) {
        const expression = firstDefined((type as UnionType).types, t => tryGetValueFromType(context, checker, importAdder, quotePreference, t, enclosingDeclaration));
        return expression ?? createUndefined();
    }
    if (checker.isArrayLikeType(type)) {
        return factory.createArrayLiteralExpression();
    }
    if (isObjectLiteralType(type)) {
        const props = map(checker.getPropertiesOfType(type), prop => {
            const initializer = tryGetValueFromType(context, checker, importAdder, quotePreference, checker.getTypeOfSymbol(prop), enclosingDeclaration);
            return factory.createPropertyAssignment(prop.name, initializer);
        });
        return factory.createObjectLiteralExpression(props, /*multiLine*/ true);
    }
    if (getObjectFlags(type) & ObjectFlags.Anonymous) {
        const decl = find(type.symbol.declarations || emptyArray, or(isFunctionTypeNode, isMethodSignature, isMethodDeclaration));
        if (decl === undefined) return createUndefined();

        const signature = checker.getSignaturesOfType(type, SignatureKind.Call);
        if (signature === undefined) return createUndefined();

        const func = createSignatureDeclarationFromSignature(SyntaxKind.FunctionExpression, context, quotePreference, signature[0],
            createStubbedBody(Diagnostics.Function_not_implemented.message, quotePreference), /*name*/ undefined, /*modifiers*/ undefined, /*optional*/ undefined, /*enclosingDeclaration*/ enclosingDeclaration, importAdder) as FunctionExpression | undefined;
        return func ?? createUndefined();
    }
    if (getObjectFlags(type) & ObjectFlags.Class) {
        const classDeclaration = getClassLikeDeclarationOfSymbol(type.symbol);
        if (classDeclaration === undefined || hasAbstractModifier(classDeclaration)) return createUndefined();

        const constructorDeclaration = getFirstConstructorWithBody(classDeclaration);
        if (constructorDeclaration && length(constructorDeclaration.parameters)) return createUndefined();

        return factory.createNewExpression(factory.createIdentifier(type.symbol.name), /*typeArguments*/ undefined, /*argumentsArray*/ undefined);
    }
    return createUndefined();
}

function createUndefined() {
    return factory.createIdentifier("undefined");
}

function isObjectLiteralType(type: Type) {
    return (type.flags & TypeFlags.Object) &&
        ((getObjectFlags(type) & ObjectFlags.ObjectLiteral) || (type.symbol && tryCast(singleOrUndefined(type.symbol.declarations), isTypeLiteralNode)));
}

/**
 * Returns an array of unmatched attributes between a JSX opening element and its contextual type.
 * @param {TypeChecker} checker - The type checker to use.
 * @param {ScriptTarget} target - The script target to use.
 * @param {JsxOpeningLikeElement} source - The JSX opening element to check.
 * @returns {Array} - An array of unmatched attributes.
 */
function getUnmatchedAttributes(checker: TypeChecker, target: ScriptTarget, source: JsxOpeningLikeElement) {
    const attrsType = checker.getContextualType(source.attributes);
    if (attrsType === undefined) return emptyArray;

    const targetProps = attrsType.getProperties();
    if (!length(targetProps)) return emptyArray;

    const seenNames = new Set<__String>();
    for (const sourceProp of source.attributes.properties) {
        if (isJsxAttribute(sourceProp)) {
            seenNames.add(getEscapedTextOfJsxAttributeName(sourceProp.name));
        }
        if (isJsxSpreadAttribute(sourceProp)) {
            const type = checker.getTypeAtLocation(sourceProp.expression);
            for (const prop of type.getProperties()) {
                seenNames.add(prop.escapedName);
            }
        }
    }
    return filter(targetProps, targetProp =>
        isIdentifierText(targetProp.name, target, LanguageVariant.JSX) && !((targetProp.flags & SymbolFlags.Optional || getCheckFlags(targetProp) & CheckFlags.Partial) || seenNames.has(targetProp.escapedName)));
}

function tryGetContainingMethodDeclaration(node: ClassLikeDeclaration | InterfaceDeclaration | TypeLiteralNode, callExpression: CallExpression) {
    if (isTypeLiteralNode(node)) {
        return undefined;
    }
    const declaration = findAncestor(callExpression, n => isMethodDeclaration(n) || isConstructorDeclaration(n));
    return declaration && declaration.parent === node ? declaration : undefined;
}

function createPropertyNameFromSymbol(symbol: Symbol, target: ScriptTarget, quotePreference: QuotePreference, checker: TypeChecker) {
    if (isTransientSymbol(symbol)) {
        const prop = checker.symbolToNode(symbol, SymbolFlags.Value, /*enclosingDeclaration*/ undefined, NodeBuilderFlags.WriteComputedProps);
        if (prop && isComputedPropertyName(prop)) return prop;
    }
    return createPropertyNameNodeForIdentifierOrLiteral(symbol.name, target, quotePreference === QuotePreference.Single);
}

function findScope(node: Node) {
    if (findAncestor(node, isJsxExpression)) {
        const returnStatement = findAncestor(node.parent, isReturnStatement);
        if (returnStatement) return returnStatement;
    }
    return getSourceFileOfNode(node);
}
