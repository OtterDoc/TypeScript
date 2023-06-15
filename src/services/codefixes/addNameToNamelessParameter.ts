import {
    Debug,
    Diagnostics,
    factory,
    getTokenAtPosition,
    Identifier,
    isParameter,
    SourceFile,
    textChanges,
} from "../_namespaces/ts";
import {
    codeFixAll,
    createCodeFixAction,
    registerCodeFix,
} from "../_namespaces/ts.codefix";

const fixId = "addNameToNamelessParameter";
const errorCodes = [Diagnostics.Parameter_has_a_name_but_no_type_Did_you_mean_0_Colon_1.code];
registerCodeFix({
    errorCodes,
    getCodeActions: function getCodeActionsToAddNameToNamelessParameter(context) {
        const changes = textChanges.ChangeTracker.with(context, t => makeChange(t, context.sourceFile, context.span.start));
        return [createCodeFixAction(fixId, changes, Diagnostics.Add_parameter_name, fixId, Diagnostics.Add_names_to_all_parameters_without_names)];
    },
    fixIds: [fixId],
    getAllCodeActions: context => codeFixAll(context, errorCodes, (changes, diag) => makeChange(changes, diag.file, diag.start)),
});

/**
 * Replaces a parameter name with a new one in a given function declaration.
 * @param {textChanges.ChangeTracker} changeTracker - The change tracker for recording the changes made.
 * @param {SourceFile} sourceFile - The source file containing the function declaration.
 * @param {number} pos - The position of the parameter to be replaced.
 * @remarks This function assumes that the parameter to be replaced is a valid parameter and does not already have a name.
 */
function makeChange(changeTracker: textChanges.ChangeTracker, sourceFile: SourceFile, pos: number) {
    const token = getTokenAtPosition(sourceFile, pos);
    const param = token.parent;
    if (!isParameter(param)) {
        return Debug.fail("Tried to add a parameter name to a non-parameter: " + Debug.formatSyntaxKind(token.kind));
    }

    const i = param.parent.parameters.indexOf(param);
    Debug.assert(!param.type, "Tried to add a parameter name to a parameter that already had one.");
    Debug.assert(i > -1, "Parameter not found in parent parameter list.");

    const typeNode = factory.createTypeReferenceNode(param.name as Identifier, /*typeArguments*/ undefined);
    const replacement = factory.createParameterDeclaration(
        param.modifiers,
        param.dotDotDotToken,
        "arg" + i,
        param.questionToken,
        param.dotDotDotToken ? factory.createArrayTypeNode(typeNode) : typeNode,
        param.initializer);
    changeTracker.replaceNode(sourceFile, param, replacement);
}
