import {
    Diagnostics,
    findAncestor,
    getTokenAtPosition,
    isCallExpression,
    textChanges,
} from "../_namespaces/ts";
import {
    createCodeFixActionWithoutFixAll,
    registerCodeFix,
} from "../_namespaces/ts.codefix";

const fixId = "removeAccidentalCallParentheses";
const errorCodes = [
    Diagnostics.This_expression_is_not_callable_because_it_is_a_get_accessor_Did_you_mean_to_use_it_without.code,
];
registerCodeFix({
    errorCodes,
    /**
     * Finds the call expression in the given context and removes the parentheses around it.
     * @param {ts.TransformationContext} context - The transformation context.
     * @returns {ts.CodeFixAction[] | undefined} An array of code fix actions or undefined if no call expression is found.
     */
    getCodeActions(context) {
        const callExpression = findAncestor(getTokenAtPosition(context.sourceFile, context.span.start), isCallExpression);
        if (!callExpression) {
            return undefined;
        }
        const changes = textChanges.ChangeTracker.with(context, t => {
            t.deleteRange(context.sourceFile, { pos: callExpression.expression.end, end: callExpression.end });
        });
        return [createCodeFixActionWithoutFixAll(fixId, changes, Diagnostics.Remove_parentheses)];
    },
    fixIds: [fixId],
});
