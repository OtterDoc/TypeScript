import {
    Diagnostics,
    hasProperty,
    quote,
    SourceFile,
    textChanges,
    UserPreferences,
} from "../_namespaces/ts";
import {
    codeFixAll,
    createCodeFixAction,
    registerCodeFix,
} from "../_namespaces/ts.codefix";

const fixIdExpression = "fixInvalidJsxCharacters_expression";
const fixIdHtmlEntity = "fixInvalidJsxCharacters_htmlEntity";

const errorCodes = [
    Diagnostics.Unexpected_token_Did_you_mean_or_gt.code,
    Diagnostics.Unexpected_token_Did_you_mean_or_rbrace.code
];

registerCodeFix({
    errorCodes,
    fixIds: [fixIdExpression, fixIdHtmlEntity],
    /**
     * Returns an array of CodeFixActions for the provided context.
     * @param {CodeFixContext} context - The context object containing sourceFile, preferences, and span properties.
     * @returns {CodeFixAction[]} An array of CodeFixActions.
     * @remarks This method uses the doChange function to create two separate text changes, one for changing invalid characters to an expression container and one for changing invalid characters to an HTML entity. It then creates two CodeFixActions using the createCodeFixAction function, one for each text change.
     */
    getCodeActions(context) {
        const { sourceFile, preferences, span } = context;
        const changeToExpression = textChanges.ChangeTracker.with(context, t => doChange(t, preferences, sourceFile, span.start, /*useHtmlEntity*/ false));
        const changeToHtmlEntity = textChanges.ChangeTracker.with(context, t => doChange(t, preferences, sourceFile, span.start, /*useHtmlEntity*/ true));

        return [
            createCodeFixAction(fixIdExpression, changeToExpression, Diagnostics.Wrap_invalid_character_in_an_expression_container, fixIdExpression, Diagnostics.Wrap_all_invalid_characters_in_an_expression_container),
            createCodeFixAction(fixIdHtmlEntity, changeToHtmlEntity, Diagnostics.Convert_invalid_character_to_its_html_entity_code, fixIdHtmlEntity, Diagnostics.Convert_all_invalid_characters_to_HTML_entity_code)
        ];
    },
    getAllCodeActions(context) {
        return codeFixAll(context, errorCodes, (changes, diagnostic) => doChange(changes, context.preferences, diagnostic.file, diagnostic.start, context.fixId === fixIdHtmlEntity));
    }
});

const htmlEntity = {
    ">": "&gt;",
    "}": "&rbrace;",
};

function isValidCharacter(character: string): character is keyof typeof htmlEntity {
    return hasProperty(htmlEntity, character);
}

/**
 * Replaces a single character in a source file with either an HTML entity or a quoted string based on user preferences.
 * @param changes - The text changes object to track changes made to the source file.
 * @param preferences - The user preferences object containing information on how to quote characters.
 * @param sourceFile - The source file to modify.
 * @param start - The position of the character to replace.
 * @param useHtmlEntity - A boolean indicating whether to use an HTML entity or a quoted string.
 */
function doChange(changes: textChanges.ChangeTracker, preferences: UserPreferences, sourceFile: SourceFile, start: number, useHtmlEntity: boolean) {
    const character = sourceFile.getText()[start];
    // sanity check
    if (!isValidCharacter(character)) {
        return;
    }

    const replacement = useHtmlEntity ? htmlEntity[character] : `{${quote(sourceFile, preferences, character)}}`;
    changes.replaceRangeWithText(sourceFile, { pos: start, end: start + 1 }, replacement);
}
