import {
    CodeFixAllContext,
    Diagnostics,
    factory,
    getJSDocTypeTag,
    getTokenAtPosition,
    idText,
    isCallExpression,
    isIdentifier,
    isInJSFile,
    isNewExpression,
    isParameter,
    isParenthesizedExpression,
    isParenthesizedTypeNode,
    isTypeReferenceNode,
    isUnionTypeNode,
    NewExpression,
    ParameterDeclaration,
    Program,
    skipTrivia,
    some,
    SourceFile,
    SyntaxKind,
    textChanges,
    TextSpan,
    TypeFlags,
} from "../_namespaces/ts";
import {
    codeFixAll,
    createCodeFixAction,
    registerCodeFix,
} from "../_namespaces/ts.codefix";

const fixName = "addVoidToPromise";
const fixId = "addVoidToPromise";
const errorCodes = [
    Diagnostics.Expected_1_argument_but_got_0_new_Promise_needs_a_JSDoc_hint_to_produce_a_resolve_that_can_be_called_without_arguments.code,
    Diagnostics.Expected_0_arguments_but_got_1_Did_you_forget_to_include_void_in_your_type_argument_to_Promise.code
];
registerCodeFix({
    errorCodes,
    fixIds: [fixId],
    getCodeActions(context) {
        const changes = textChanges.ChangeTracker.with(context, t => makeChange(t, context.sourceFile, context.span, context.program));
        if (changes.length > 0) {
            return [createCodeFixAction(fixName, changes, Diagnostics.Add_void_to_Promise_resolved_without_a_value, fixId, Diagnostics.Add_void_to_all_Promises_resolved_without_a_value)];
        }
    },
    getAllCodeActions(context: CodeFixAllContext) {
        return codeFixAll(context, errorCodes, (changes, diag) => makeChange(changes, diag.file, diag, context.program, new Set()));
    }
});

/**
 * Modifies a Promise declaration to include a `void` type argument or append ` | void` to an existing type argument.
 * @param {textChanges.ChangeTracker} changes - The change tracker to use for modifying the source file.
 * @param {SourceFile} sourceFile - The source file containing the Promise declaration.
 * @param {TextSpan} span - The text span of the Promise declaration.
 * @param {Program} program - The program containing the source file.
 * @param {Set<ParameterDeclaration>} [seen] - A set of previously seen ParameterDeclarations to avoid making duplicate changes.
 */
function makeChange(changes: textChanges.ChangeTracker, sourceFile: SourceFile, span: TextSpan, program: Program, seen?: Set<ParameterDeclaration>) {
    const node = getTokenAtPosition(sourceFile, span.start);
    if (!isIdentifier(node) || !isCallExpression(node.parent) || node.parent.expression !== node || node.parent.arguments.length !== 0) return;

    const checker = program.getTypeChecker();
    const symbol = checker.getSymbolAtLocation(node);

    // decl should be `new Promise((<decl>) => {})`
    const decl = symbol?.valueDeclaration;
    if (!decl || !isParameter(decl) || !isNewExpression(decl.parent.parent)) return;

    // no need to make this change if we have already seen this parameter.
    if (seen?.has(decl)) return;
    seen?.add(decl);

    const typeArguments = getEffectiveTypeArguments(decl.parent.parent);
    if (some(typeArguments)) {
        // append ` | void` to type argument
        const typeArgument = typeArguments[0];
        const needsParens = !isUnionTypeNode(typeArgument) && !isParenthesizedTypeNode(typeArgument) &&
            isParenthesizedTypeNode(factory.createUnionTypeNode([typeArgument, factory.createKeywordTypeNode(SyntaxKind.VoidKeyword)]).types[0]);
        if (needsParens) {
            changes.insertText(sourceFile, typeArgument.pos, "(");
        }
        changes.insertText(sourceFile, typeArgument.end, needsParens ? ") | void" : " | void");
    }
    else {
        // make sure the Promise is type is untyped (i.e., `unknown`)
        const signature = checker.getResolvedSignature(node.parent);
        const parameter = signature?.parameters[0];
        const parameterType = parameter && checker.getTypeOfSymbolAtLocation(parameter, decl.parent.parent);
        if (isInJSFile(decl)) {
            if (!parameterType || parameterType.flags & TypeFlags.AnyOrUnknown) {
                // give the expression a type
                changes.insertText(sourceFile, decl.parent.parent.end, `)`);
                changes.insertText(sourceFile, skipTrivia(sourceFile.text, decl.parent.parent.pos), `/** @type {Promise<void>} */(`);
            }
        }
        else {
            if (!parameterType || parameterType.flags & TypeFlags.Unknown) {
                // add `void` type argument
                changes.insertText(sourceFile, decl.parent.parent.expression.end, "<void>");
            }
        }
    }
}

/**
 * Returns the effective type arguments of a NewExpression node. If the node is in a JavaScript file and is a Promise type wrapped in a ParenthesizedExpression with a JSDocType tag, returns the type arguments from the JSDocType. Otherwise, returns the type arguments from the node.
 * @param {NewExpression} node - The NewExpression node to get the effective type arguments from.
 * @returns {NodeArray<TypeNode> | undefined} - The effective type arguments of the node, or undefined if none are found.
 */
function getEffectiveTypeArguments(node: NewExpression) {
    if (isInJSFile(node)) {
        if (isParenthesizedExpression(node.parent)) {
            const jsDocType = getJSDocTypeTag(node.parent)?.typeExpression.type;
            if (jsDocType && isTypeReferenceNode(jsDocType) && isIdentifier(jsDocType.typeName) && idText(jsDocType.typeName) === "Promise") {
                return jsDocType.typeArguments;
            }
        }
    }
    else {
        return node.typeArguments;
    }
}
