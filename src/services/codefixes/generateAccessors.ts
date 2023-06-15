import {
    AccessorDeclaration,
    canHaveDecorators,
    cast,
    ClassLikeDeclaration,
    concatenate,
    ConstructorDeclaration,
    DeclarationName,
    Diagnostics,
    factory,
    FileTextChanges,
    find,
    findAncestor,
    getClassExtendsHeritageElement,
    getDecorators,
    getEffectiveModifierFlags,
    getFirstConstructorWithBody,
    getLocaleSpecificMessage,
    getTokenAtPosition,
    getTypeAnnotationNode,
    getUniqueName,
    hasEffectiveReadonlyModifier,
    hasStaticModifier,
    Identifier,
    InterfaceDeclaration,
    isClassLike,
    isElementAccessExpression,
    isFunctionLike,
    isIdentifier,
    isParameterPropertyDeclaration,
    isPropertyAccessExpression,
    isPropertyAssignment,
    isPropertyDeclaration,
    isSourceFileJS,
    isStringLiteral,
    isUnionTypeNode,
    isWriteAccess,
    ModifierFlags,
    ModifierLike,
    Mutable,
    Node,
    nodeOverlapsWithStartEnd,
    ObjectLiteralExpression,
    ParameterPropertyDeclaration,
    Program,
    PropertyAssignment,
    PropertyDeclaration,
    refactor,
    SourceFile,
    startsWithUnderscore,
    StringLiteral,
    suppressLeadingAndTrailingTrivia,
    SymbolFlags,
    SyntaxKind,
    textChanges,
    TypeChecker,
    TypeNode,
} from "../_namespaces/ts";

/** @internal */
export type AcceptedDeclaration = ParameterPropertyDeclaration | PropertyDeclaration | PropertyAssignment;
/** @internal */
export type AcceptedNameType = Identifier | StringLiteral;
/** @internal */
export type ContainerDeclaration = ClassLikeDeclaration | ObjectLiteralExpression;

/** @internal */
export type AccessorOrRefactorErrorInfo = AccessorInfo | refactor.RefactorErrorInfo;
/** @internal */
export interface AccessorInfo {
    readonly container: ContainerDeclaration;
    readonly isStatic: boolean;
    readonly isReadonly: boolean;
    readonly type: TypeNode | undefined;
    readonly declaration: AcceptedDeclaration;
    readonly fieldName: AcceptedNameType;
    readonly accessorName: AcceptedNameType;
    readonly originalName: string;
    readonly renameAccessor: boolean;
}

/**
 * Generates an accessor from a property.
 * @param {SourceFile} file - The source file.
 * @param {Program} program - The program.
 * @param {number} start - The start position.
 * @param {number} end - The end position.
 * @param {textChanges.TextChangesContext} context - The text changes context.
 * @param {string} _actionName - The action name.
 * @returns {FileTextChanges[] | undefined} - The file text changes or undefined.
 * @internal
 */
export function generateAccessorFromProperty(file: SourceFile, program: Program, start: number, end: number, context: textChanges.TextChangesContext, _actionName: string): FileTextChanges[] | undefined {
    const fieldInfo = getAccessorConvertiblePropertyAtPosition(file, program, start, end);
    if (!fieldInfo || refactor.isRefactorErrorInfo(fieldInfo)) return undefined;

    const changeTracker = textChanges.ChangeTracker.fromContext(context);
    const { isStatic, isReadonly, fieldName, accessorName, originalName, type, container, declaration } = fieldInfo;

    suppressLeadingAndTrailingTrivia(fieldName);
    suppressLeadingAndTrailingTrivia(accessorName);
    suppressLeadingAndTrailingTrivia(declaration);
    suppressLeadingAndTrailingTrivia(container);

    let accessorModifiers: readonly ModifierLike[] | undefined;
    let fieldModifiers: readonly ModifierLike[] | undefined;
    if (isClassLike(container)) {
        const modifierFlags = getEffectiveModifierFlags(declaration);
        if (isSourceFileJS(file)) {
            const modifiers = factory.createModifiersFromModifierFlags(modifierFlags);
            accessorModifiers = modifiers;
            fieldModifiers = modifiers;
        }
        else {
            accessorModifiers = factory.createModifiersFromModifierFlags(prepareModifierFlagsForAccessor(modifierFlags));
            fieldModifiers = factory.createModifiersFromModifierFlags(prepareModifierFlagsForField(modifierFlags));
        }
        if (canHaveDecorators(declaration)) {
            fieldModifiers = concatenate(getDecorators(declaration), fieldModifiers);
        }
    }

    updateFieldDeclaration(changeTracker, file, declaration, type, fieldName, fieldModifiers);

    const getAccessor = generateGetAccessor(fieldName, accessorName, type, accessorModifiers, isStatic, container);
    suppressLeadingAndTrailingTrivia(getAccessor);
    insertAccessor(changeTracker, file, getAccessor, declaration, container);

    if (isReadonly) {
        // readonly modifier only existed in classLikeDeclaration
        const constructor = getFirstConstructorWithBody(container as ClassLikeDeclaration);
        if (constructor) {
            updateReadonlyPropertyInitializerStatementConstructor(changeTracker, file, constructor, fieldName.text, originalName);
        }
    }
    else {
        const setAccessor = generateSetAccessor(fieldName, accessorName, type, accessorModifiers, isStatic, container);
        suppressLeadingAndTrailingTrivia(setAccessor);
        insertAccessor(changeTracker, file, setAccessor, declaration, container);
    }

    return changeTracker.getChanges();
}

function isConvertibleName(name: DeclarationName): name is AcceptedNameType {
    return isIdentifier(name) || isStringLiteral(name);
}

function isAcceptedDeclaration(node: Node): node is AcceptedDeclaration {
    return isParameterPropertyDeclaration(node, node.parent) || isPropertyDeclaration(node) || isPropertyAssignment(node);
}

function createPropertyName(name: string, originalName: AcceptedNameType) {
    return isIdentifier(originalName) ? factory.createIdentifier(name) : factory.createStringLiteral(name);
}

function createAccessorAccessExpression(fieldName: AcceptedNameType, isStatic: boolean, container: ContainerDeclaration) {
    const leftHead = isStatic ? (container as ClassLikeDeclaration).name! : factory.createThis(); // TODO: GH#18217
    return isIdentifier(fieldName) ? factory.createPropertyAccessExpression(leftHead, fieldName) : factory.createElementAccessExpression(leftHead, factory.createStringLiteralFromNode(fieldName));
}

/**
 * Prepares modifier flags for an accessor.
 * @param modifierFlags - The modifier flags to prepare.
 * @returns The modified modifier flags.
 */
function prepareModifierFlagsForAccessor(modifierFlags: ModifierFlags): ModifierFlags {
    modifierFlags &= ~ModifierFlags.Readonly; // avoid Readonly modifier because it will convert to get accessor
    modifierFlags &= ~ModifierFlags.Private;

    if (!(modifierFlags & ModifierFlags.Protected)) {
        modifierFlags |= ModifierFlags.Public;
    }

    return modifierFlags;
}

function prepareModifierFlagsForField(modifierFlags: ModifierFlags): ModifierFlags {
    modifierFlags &= ~ModifierFlags.Public;
    modifierFlags &= ~ModifierFlags.Protected;
    modifierFlags |= ModifierFlags.Private;
    return modifierFlags;
}

/**
 * Retrieves information about a property for which to generate an accessor.
 * @param file - The source file containing the property.
 * @param program - The program containing the file.
 * @param start - The start position of the property.
 * @param end - The end position of the property.
 * @param considerEmptySpans - Optional flag indicating whether to consider empty spans.
 * @returns An object containing information about the property, or undefined if not found.
 */
export function getAccessorConvertiblePropertyAtPosition(file: SourceFile, program: Program, start: number, end: number, considerEmptySpans = true): AccessorOrRefactorErrorInfo | undefined {
    const node = getTokenAtPosition(file, start);
    const cursorRequest = start === end && considerEmptySpans;
    const declaration = findAncestor(node.parent, isAcceptedDeclaration);
    // make sure declaration have AccessibilityModifier or Static Modifier or Readonly Modifier
    const meaning = ModifierFlags.AccessibilityModifier | ModifierFlags.Static | ModifierFlags.Readonly;

    if (!declaration || (!(nodeOverlapsWithStartEnd(declaration.name, file, start, end) || cursorRequest))) {
        return {
            error: getLocaleSpecificMessage(Diagnostics.Could_not_find_property_for_which_to_generate_accessor)
        };
    }

    if (!isConvertibleName(declaration.name)) {
        return {
            error: getLocaleSpecificMessage(Diagnostics.Name_is_not_valid)
        };
    }

    if (((getEffectiveModifierFlags(declaration) & ModifierFlags.Modifier) | meaning) !== meaning) {
        return {
            error: getLocaleSpecificMessage(Diagnostics.Can_only_convert_property_with_modifier)
        };
    }

    const name = declaration.name.text;
    const startWithUnderscore = startsWithUnderscore(name);
    const fieldName = createPropertyName(startWithUnderscore ? name : getUniqueName(`_${name}`, file), declaration.name);
    const accessorName = createPropertyName(startWithUnderscore ? getUniqueName(name.substring(1), file) : name, declaration.name);
    return {
        isStatic: hasStaticModifier(declaration),
        isReadonly: hasEffectiveReadonlyModifier(declaration),
        type: getDeclarationType(declaration, program),
        container: declaration.kind === SyntaxKind.Parameter ? declaration.parent.parent : declaration.parent,
        originalName: (declaration.name as AcceptedNameType).text,
        declaration,
        fieldName,
        accessorName,
        renameAccessor: startWithUnderscore
    };
}

/**
 * Generates a get accessor declaration for a given field name, accessor name, type, modifiers, static flag, and container declaration.
 * @param {AcceptedNameType} fieldName - The name of the field to generate the accessor for.
 * @param {AcceptedNameType} accessorName - The name of the accessor to generate.
 * @param {TypeNode | undefined} type - The type of the accessor.
 * @param {readonly ModifierLike[] | undefined} modifiers - The modifiers for the accessor.
 * @param {boolean} isStatic - Whether the accessor is static or not.
 * @param {ContainerDeclaration} container - The container declaration for the accessor.
 * @returns {GetAccessorDeclaration} - The generated get accessor declaration.
 */
function generateGetAccessor(fieldName: AcceptedNameType, accessorName: AcceptedNameType, type: TypeNode | undefined, modifiers: readonly ModifierLike[] | undefined, isStatic: boolean, container: ContainerDeclaration) {
    return factory.createGetAccessorDeclaration(
        modifiers,
        accessorName,
        [],
        type,
        factory.createBlock([
            factory.createReturnStatement(
                createAccessorAccessExpression(fieldName, isStatic, container)
            )
        ], /*multiLine*/ true)
    );
}

/**
 * Generates a set accessor declaration.
 * @param fieldName - The name of the field to set.
 * @param accessorName - The name of the accessor.
 * @param type - The type of the parameter.
 * @param modifiers - The modifiers for the accessor.
 * @param isStatic - Indicates if the accessor is static.
 * @param container - The container declaration.
 * @returns The set accessor declaration.
 */
function generateSetAccessor(fieldName: AcceptedNameType, accessorName: AcceptedNameType, type: TypeNode | undefined, modifiers: readonly ModifierLike[] | undefined, isStatic: boolean, container: ContainerDeclaration) {
    return factory.createSetAccessorDeclaration(
        modifiers,
        accessorName,
        [factory.createParameterDeclaration(
            /*modifiers*/ undefined,
            /*dotDotDotToken*/ undefined,
            factory.createIdentifier("value"),
            /*questionToken*/ undefined,
            type
        )],
        factory.createBlock([
            factory.createExpressionStatement(
                factory.createAssignment(
                    createAccessorAccessExpression(fieldName, isStatic, container),
                    factory.createIdentifier("value")
                )
            )
        ], /*multiLine*/ true)
    );
}

/**
 * Updates a PropertyDeclaration node in a SourceFile with the provided information.
 * @param changeTracker - The ChangeTracker object used to track changes made to the SourceFile.
 * @param file - The SourceFile containing the PropertyDeclaration to be updated.
 * @param declaration - The PropertyDeclaration to be updated.
 * @param type - The TypeNode to be assigned to the PropertyDeclaration.
 * @param fieldName - The name to be assigned to the PropertyDeclaration.
 * @param modifiers - An array of ModifierLike objects to be assigned to the PropertyDeclaration.
 */
function updatePropertyDeclaration(changeTracker: textChanges.ChangeTracker, file: SourceFile, declaration: PropertyDeclaration, type: TypeNode | undefined, fieldName: AcceptedNameType, modifiers: readonly ModifierLike[] | undefined) {
    const property = factory.updatePropertyDeclaration(
        declaration,
        modifiers,
        fieldName,
        declaration.questionToken || declaration.exclamationToken,
        type,
        declaration.initializer
    );
    changeTracker.replaceNode(file, declaration, property);
}

/**
 * Updates a PropertyAssignment declaration with a new field name and returns the updated assignment.
 * @param changeTracker - The ChangeTracker object used to track changes.
 * @param file - The SourceFile object containing the declaration.
 * @param declaration - The PropertyAssignment object to update.
 * @param fieldName - The new field name to assign to the PropertyAssignment.
 * @returns The updated PropertyAssignment object.
 */
function updatePropertyAssignmentDeclaration(changeTracker: textChanges.ChangeTracker, file: SourceFile, declaration: PropertyAssignment, fieldName: AcceptedNameType) {
    let assignment = factory.updatePropertyAssignment(declaration, fieldName, declaration.initializer);
    // Remove grammar errors from assignment
    if (assignment.modifiers || assignment.questionToken || assignment.exclamationToken) {
        if (assignment === declaration) assignment = factory.cloneNode(assignment);
        (assignment as Mutable<PropertyAssignment>).modifiers = undefined;
        (assignment as Mutable<PropertyAssignment>).questionToken = undefined;
        (assignment as Mutable<PropertyAssignment>).exclamationToken = undefined;
    }
    changeTracker.replacePropertyAssignment(file, declaration, assignment);
}

/**
 * Updates a field declaration in a TypeScript source file.
 * @param changeTracker - The change tracker to use for updating the file.
 * @param file - The source file containing the declaration to update.
 * @param declaration - The declaration to update.
 * @param type - The type of the field, if any.
 * @param fieldName - The name of the field.
 * @param modifiers - The modifiers applied to the field, if any.
 * @remarks This function can update both property declarations and property assignments.
 */
function updateFieldDeclaration(changeTracker: textChanges.ChangeTracker, file: SourceFile, declaration: AcceptedDeclaration, type: TypeNode | undefined, fieldName: AcceptedNameType, modifiers: readonly ModifierLike[] | undefined) {
    if (isPropertyDeclaration(declaration)) {
        updatePropertyDeclaration(changeTracker, file, declaration, type, fieldName, modifiers);
    }
    else if (isPropertyAssignment(declaration)) {
        updatePropertyAssignmentDeclaration(changeTracker, file, declaration, fieldName);
    }
    else {
        changeTracker.replaceNode(file, declaration,
            factory.updateParameterDeclaration(declaration, modifiers, declaration.dotDotDotToken, cast(fieldName, isIdentifier), declaration.questionToken, declaration.type, declaration.initializer));
    }
}

function insertAccessor(changeTracker: textChanges.ChangeTracker, file: SourceFile, accessor: AccessorDeclaration, declaration: AcceptedDeclaration, container: ContainerDeclaration) {
    isParameterPropertyDeclaration(declaration, declaration.parent) ? changeTracker.insertMemberAtStart(file, container as ClassLikeDeclaration, accessor) :
        isPropertyAssignment(declaration) ? changeTracker.insertNodeAfterComma(file, declaration, accessor) :
        changeTracker.insertNodeAfter(file, declaration, accessor);
}

/**
 * Updates the initializer statement of a readonly property constructor with a new field name.
 * @param {textChanges.ChangeTracker} changeTracker - The change tracker to use for updating the code.
 * @param {SourceFile} file - The source file containing the constructor.
 * @param {ConstructorDeclaration} constructor - The constructor to update.
 * @param {string} fieldName - The new field name to use.
 * @param {string} originalName - The original name of the field.
 */
function updateReadonlyPropertyInitializerStatementConstructor(changeTracker: textChanges.ChangeTracker, file: SourceFile, constructor: ConstructorDeclaration, fieldName: string, originalName: string) {
    if (!constructor.body) return;
    constructor.body.forEachChild(function recur(node) {
        if (isElementAccessExpression(node) &&
            node.expression.kind === SyntaxKind.ThisKeyword &&
            isStringLiteral(node.argumentExpression) &&
            node.argumentExpression.text === originalName &&
            isWriteAccess(node)) {
            changeTracker.replaceNode(file, node.argumentExpression, factory.createStringLiteral(fieldName));
        }
        if (isPropertyAccessExpression(node) && node.expression.kind === SyntaxKind.ThisKeyword && node.name.text === originalName && isWriteAccess(node)) {
            changeTracker.replaceNode(file, node.name, factory.createIdentifier(fieldName));
        }
        if (!isFunctionLike(node) && !isClassLike(node)) {
            node.forEachChild(recur);
        }
    });
}

/**
 * Retrieves the type annotation node for a given accepted declaration and program.
 * @param {AcceptedDeclaration} declaration - The accepted declaration to retrieve the type annotation node for.
 * @param {Program} program - The program to retrieve the type checker from.
 * @returns {TypeNode | undefined} - The type annotation node for the given declaration, or undefined if none exists.
 */
function getDeclarationType(declaration: AcceptedDeclaration, program: Program): TypeNode | undefined {
    const typeNode = getTypeAnnotationNode(declaration);
    if (isPropertyDeclaration(declaration) && typeNode && declaration.questionToken) {
        const typeChecker = program.getTypeChecker();
        const type = typeChecker.getTypeFromTypeNode(typeNode);
        if (!typeChecker.isTypeAssignableTo(typeChecker.getUndefinedType(), type)) {
            const types = isUnionTypeNode(typeNode) ? typeNode.types : [typeNode];
            return factory.createUnionTypeNode([...types, factory.createKeywordTypeNode(SyntaxKind.UndefinedKeyword)]);
        }
    }
    return typeNode;
}

/**
 * Returns an array of all super classes and interfaces of the given class or interface.
 * @param decl - The class or interface to get the super classes and interfaces of.
 * @param checker - The TypeChecker instance to use for symbol resolution.
 * @returns An array of ClassOrInterface instances representing the super classes and interfaces of the given class or interface.
 * @remarks This function is intended for internal use only.
 */
export function getAllSupers(decl: ClassOrInterface | undefined, checker: TypeChecker): readonly ClassOrInterface[] {
    const res: ClassLikeDeclaration[] = [];
    while (decl) {
        const superElement = getClassExtendsHeritageElement(decl);
        const superSymbol = superElement && checker.getSymbolAtLocation(superElement.expression);
        if (!superSymbol) break;
        const symbol = superSymbol.flags & SymbolFlags.Alias ? checker.getAliasedSymbol(superSymbol) : superSymbol;
        const superDecl = symbol.declarations && find(symbol.declarations, isClassLike);
        if (!superDecl) break;
        res.push(superDecl);
        decl = superDecl;
    }
    return res;
}

/** @internal */
export type ClassOrInterface = ClassLikeDeclaration | InterfaceDeclaration;
