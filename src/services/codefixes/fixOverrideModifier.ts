import {
    CodeFixAllContext,
    CodeFixContext,
    ConstructorDeclaration,
    Debug,
    DiagnosticMessage,
    Diagnostics,
    emptyArray,
    factory,
    find,
    findAncestor,
    findLast,
    GetAccessorDeclaration,
    getTokenAtPosition,
    isAbstractModifier,
    isAccessibilityModifier,
    isClassLike,
    isDecorator,
    isJSDocOverrideTag,
    isOverrideModifier,
    isParameterPropertyDeclaration,
    isSourceFileJS,
    isStaticModifier,
    MethodDeclaration,
    Node,
    not,
    ParameterPropertyDeclaration,
    PropertyDeclaration,
    SetAccessorDeclaration,
    skipTrivia,
    SourceFile,
    SyntaxKind,
    textChanges,
} from "../_namespaces/ts";
import {
    codeFixAll,
    createCodeFixActionMaybeFixAll,
    registerCodeFix,
} from "../_namespaces/ts.codefix";

const fixName = "fixOverrideModifier";
const fixAddOverrideId = "fixAddOverrideModifier";
const fixRemoveOverrideId = "fixRemoveOverrideModifier";

type ClassElementLikeHasJSDoc =
    | ConstructorDeclaration
    | PropertyDeclaration
    | MethodDeclaration
    | GetAccessorDeclaration
    | SetAccessorDeclaration
    | ParameterPropertyDeclaration;

const errorCodes = [
    Diagnostics.This_member_cannot_have_an_override_modifier_because_it_is_not_declared_in_the_base_class_0.code,
    Diagnostics.This_member_cannot_have_an_override_modifier_because_its_containing_class_0_does_not_extend_another_class.code,
    Diagnostics.This_member_must_have_an_override_modifier_because_it_overrides_an_abstract_method_that_is_declared_in_the_base_class_0.code,
    Diagnostics.This_member_must_have_an_override_modifier_because_it_overrides_a_member_in_the_base_class_0.code,
    Diagnostics.This_parameter_property_must_have_an_override_modifier_because_it_overrides_a_member_in_base_class_0.code,
    Diagnostics.This_member_must_have_a_JSDoc_comment_with_an_override_tag_because_it_overrides_a_member_in_the_base_class_0.code,
    Diagnostics.This_member_cannot_have_a_JSDoc_comment_with_an_override_tag_because_its_containing_class_0_does_not_extend_another_class.code,
    Diagnostics.This_parameter_property_must_have_a_JSDoc_comment_with_an_override_tag_because_it_overrides_a_member_in_the_base_class_0.code,
    Diagnostics.This_member_cannot_have_a_JSDoc_comment_with_an_override_tag_because_it_is_not_declared_in_the_base_class_0.code,
];

interface ErrorCodeFixInfo {
    descriptions: DiagnosticMessage;
    fixId?: string | undefined;
    fixAllDescriptions?: DiagnosticMessage | undefined;
}

const errorCodeFixIdMap: Record<number, ErrorCodeFixInfo> = {
    // case #1:
    [Diagnostics.This_member_must_have_an_override_modifier_because_it_overrides_a_member_in_the_base_class_0.code]: {
        descriptions: Diagnostics.Add_override_modifier,
        fixId: fixAddOverrideId,
        fixAllDescriptions: Diagnostics.Add_all_missing_override_modifiers,
    },
    [Diagnostics.This_member_must_have_a_JSDoc_comment_with_an_override_tag_because_it_overrides_a_member_in_the_base_class_0.code]: {
        descriptions: Diagnostics.Add_override_modifier,
        fixId: fixAddOverrideId,
        fixAllDescriptions: Diagnostics.Add_all_missing_override_modifiers
    },
    // case #2:
    [Diagnostics.This_member_cannot_have_an_override_modifier_because_its_containing_class_0_does_not_extend_another_class.code]: {
        descriptions: Diagnostics.Remove_override_modifier,
        fixId: fixRemoveOverrideId,
        fixAllDescriptions: Diagnostics.Remove_all_unnecessary_override_modifiers,
    },
    [Diagnostics.This_member_cannot_have_a_JSDoc_comment_with_an_override_tag_because_its_containing_class_0_does_not_extend_another_class.code]: {
        descriptions: Diagnostics.Remove_override_modifier,
        fixId: fixRemoveOverrideId,
        fixAllDescriptions: Diagnostics.Remove_override_modifier
    },
    // case #3:
    [Diagnostics.This_parameter_property_must_have_an_override_modifier_because_it_overrides_a_member_in_base_class_0.code]: {
        descriptions: Diagnostics.Add_override_modifier,
        fixId: fixAddOverrideId,
        fixAllDescriptions: Diagnostics.Add_all_missing_override_modifiers,
    },
    [Diagnostics.This_parameter_property_must_have_a_JSDoc_comment_with_an_override_tag_because_it_overrides_a_member_in_the_base_class_0.code]: {
        descriptions: Diagnostics.Add_override_modifier,
        fixId: fixAddOverrideId,
        fixAllDescriptions: Diagnostics.Add_all_missing_override_modifiers,
    },
    // case #4:
    [Diagnostics.This_member_must_have_an_override_modifier_because_it_overrides_an_abstract_method_that_is_declared_in_the_base_class_0.code]: {
        descriptions: Diagnostics.Add_override_modifier,
        fixId: fixAddOverrideId,
        fixAllDescriptions: Diagnostics.Remove_all_unnecessary_override_modifiers,
    },
    // case #5:
    [Diagnostics.This_member_cannot_have_an_override_modifier_because_it_is_not_declared_in_the_base_class_0.code]: {
        descriptions: Diagnostics.Remove_override_modifier,
        fixId: fixRemoveOverrideId,
        fixAllDescriptions: Diagnostics.Remove_all_unnecessary_override_modifiers,
    },
    [Diagnostics.This_member_cannot_have_a_JSDoc_comment_with_an_override_tag_because_it_is_not_declared_in_the_base_class_0.code]: {
        descriptions: Diagnostics.Remove_override_modifier,
        fixId: fixRemoveOverrideId,
        fixAllDescriptions: Diagnostics.Remove_all_unnecessary_override_modifiers,
    }
};

registerCodeFix({
    errorCodes,
    getCodeActions: function getCodeActionsToFixOverrideModifierIssues(context) {
        const { errorCode, span } = context;

        const info = errorCodeFixIdMap[errorCode];
        if (!info) return emptyArray;

        const { descriptions, fixId, fixAllDescriptions } = info;
        const changes = textChanges.ChangeTracker.with(context, changes => dispatchChanges(changes, context, errorCode, span.start));

        return [
            createCodeFixActionMaybeFixAll(fixName, changes, descriptions, fixId, fixAllDescriptions)
        ];
    },
    fixIds: [fixName, fixAddOverrideId, fixRemoveOverrideId],
    getAllCodeActions: context =>
        codeFixAll(context, errorCodes, (changes, diag) => {
            const { code, start } = diag;
            const info = errorCodeFixIdMap[code];
            if (!info || info.fixId !== context.fixId) {
                return;
            }

            dispatchChanges(changes, context, code, start);
        })
});

/**
 * Dispatches changes based on the error code.
 * @param {textChanges.ChangeTracker} changeTracker - The change tracker.
 * @param {CodeFixContext | CodeFixAllContext} context - The code fix context.
 * @param {number} errorCode - The error code.
 * @param {number} pos - The position.
 */
function dispatchChanges(
    changeTracker: textChanges.ChangeTracker,
    context: CodeFixContext | CodeFixAllContext,
    errorCode: number,
    pos: number) {
    switch (errorCode) {
        case Diagnostics.This_member_must_have_an_override_modifier_because_it_overrides_a_member_in_the_base_class_0.code:
        case Diagnostics.This_member_must_have_a_JSDoc_comment_with_an_override_tag_because_it_overrides_a_member_in_the_base_class_0.code:
        case Diagnostics.This_member_must_have_an_override_modifier_because_it_overrides_an_abstract_method_that_is_declared_in_the_base_class_0.code:
        case Diagnostics.This_parameter_property_must_have_an_override_modifier_because_it_overrides_a_member_in_base_class_0.code:
        case Diagnostics.This_parameter_property_must_have_a_JSDoc_comment_with_an_override_tag_because_it_overrides_a_member_in_the_base_class_0.code:
            return doAddOverrideModifierChange(changeTracker, context.sourceFile, pos);
        case Diagnostics.This_member_cannot_have_an_override_modifier_because_it_is_not_declared_in_the_base_class_0.code:
        case Diagnostics.This_member_cannot_have_a_JSDoc_comment_with_an_override_tag_because_it_is_not_declared_in_the_base_class_0.code:
        case Diagnostics.This_member_cannot_have_an_override_modifier_because_its_containing_class_0_does_not_extend_another_class.code:
        case Diagnostics.This_member_cannot_have_a_JSDoc_comment_with_an_override_tag_because_its_containing_class_0_does_not_extend_another_class.code:
            return doRemoveOverrideModifierChange(changeTracker, context.sourceFile, pos);
        default:
            Debug.fail("Unexpected error code: " + errorCode);
    }
}

/**
 * Adds an override modifier to a class element and updates the source file using a change tracker.
 * @param changeTracker - The change tracker to use for updating the source file.
 * @param sourceFile - The source file to update.
 * @param pos - The position of the class element to modify.
 */
function doAddOverrideModifierChange(changeTracker: textChanges.ChangeTracker, sourceFile: SourceFile, pos: number) {
    const classElement = findContainerClassElementLike(sourceFile, pos);
    if (isSourceFileJS(sourceFile)) {
        changeTracker.addJSDocTags(sourceFile, classElement, [factory.createJSDocOverrideTag(factory.createIdentifier("override"))]);
        return;
    }
    const modifiers = classElement.modifiers || emptyArray;
    const staticModifier = find(modifiers, isStaticModifier);
    const abstractModifier = find(modifiers, isAbstractModifier);
    const accessibilityModifier = find(modifiers, m => isAccessibilityModifier(m.kind));
    const lastDecorator = findLast(modifiers, isDecorator);
    const modifierPos = abstractModifier ? abstractModifier.end :
        staticModifier ? staticModifier.end :
        accessibilityModifier ? accessibilityModifier.end :
        lastDecorator ? skipTrivia(sourceFile.text, lastDecorator.end) : classElement.getStart(sourceFile);
    const options = accessibilityModifier || staticModifier || abstractModifier ? { prefix: " " } : { suffix: " " };
    changeTracker.insertModifierAt(sourceFile, modifierPos, SyntaxKind.OverrideKeyword, options);
}

/**
 * Removes the override modifier from a class element and updates the change tracker.
 * @param {textChanges.ChangeTracker} changeTracker - The change tracker to update.
 * @param {SourceFile} sourceFile - The source file containing the class element.
 * @param {number} pos - The position of the class element in the source file.
 */
function doRemoveOverrideModifierChange(changeTracker: textChanges.ChangeTracker, sourceFile: SourceFile, pos: number) {
    const classElement = findContainerClassElementLike(sourceFile, pos);
    if (isSourceFileJS(sourceFile)) {
        changeTracker.filterJSDocTags(sourceFile, classElement, not(isJSDocOverrideTag));
        return;
    }
    const overrideModifier = find(classElement.modifiers, isOverrideModifier);
    Debug.assertIsDefined(overrideModifier);

    changeTracker.deleteModifier(sourceFile, overrideModifier);
}

/**
 * Determines if a given node is a class element with JSDoc comments.
 * @param {Node} node - The node to check.
 * @returns {boolean} - True if the node is a class element with JSDoc comments, false otherwise.
 */
function isClassElementLikeHasJSDoc(node: Node): node is ClassElementLikeHasJSDoc {
    switch (node.kind) {
        case SyntaxKind.Constructor:
        case SyntaxKind.PropertyDeclaration:
        case SyntaxKind.MethodDeclaration:
        case SyntaxKind.GetAccessor:
        case SyntaxKind.SetAccessor:
            return true;
        case SyntaxKind.Parameter:
            return isParameterPropertyDeclaration(node, node.parent);
        default:
            return false;
    }
}

/**
 * Finds the nearest class element with JSDoc comments in a given TypeScript source file at a specified position.
 * @param {SourceFile} sourceFile - The TypeScript source file to search in.
 * @param {number} pos - The position to start searching from.
 * @returns {Node | undefined} - The nearest class element with JSDoc comments, or undefined if none is found.
 */
function findContainerClassElementLike(sourceFile: SourceFile, pos: number) {
    const token = getTokenAtPosition(sourceFile, pos);
    const classElement = findAncestor(token, node => {
        if (isClassLike(node)) return "quit";
        return isClassElementLikeHasJSDoc(node);
    });

    Debug.assert(classElement && isClassElementLikeHasJSDoc(classElement));
    return classElement;
}

