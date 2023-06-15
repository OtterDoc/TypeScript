import {
    addToSeen,
    CodeFixContextBase,
    contains,
    createTextSpanFromNode,
    Diagnostics,
    ExportSpecifier,
    factory,
    filter,
    findDiagnosticForNode,
    getDiagnosticsWithinSpan,
    getNodeId,
    getTokenAtPosition,
    isExportSpecifier,
    SourceFile,
    SyntaxKind,
    textChanges,
    TextSpan,
    tryCast,
} from "../_namespaces/ts";
import {
    codeFixAll,
    createCodeFixAction,
    registerCodeFix,
} from "../_namespaces/ts.codefix";

const errorCodes = [Diagnostics.Re_exporting_a_type_when_0_is_enabled_requires_using_export_type.code];
const fixId = "convertToTypeOnlyExport";
registerCodeFix({
    errorCodes,
    getCodeActions: function getCodeActionsToConvertToTypeOnlyExport(context) {
        const changes = textChanges.ChangeTracker.with(context, t => fixSingleExportDeclaration(t, getExportSpecifierForDiagnosticSpan(context.span, context.sourceFile), context));
        if (changes.length) {
            return [createCodeFixAction(fixId, changes, Diagnostics.Convert_to_type_only_export, fixId, Diagnostics.Convert_all_re_exported_types_to_type_only_exports)];
        }
    },
    fixIds: [fixId],
    getAllCodeActions: function getAllCodeActionsToConvertToTypeOnlyExport(context) {
        const fixedExportDeclarations = new Map<number, true>();
        return codeFixAll(context, errorCodes, (changes, diag) => {
            const exportSpecifier = getExportSpecifierForDiagnosticSpan(diag, context.sourceFile);
            if (exportSpecifier && addToSeen(fixedExportDeclarations, getNodeId(exportSpecifier.parent.parent))) {
                fixSingleExportDeclaration(changes, exportSpecifier, context);
            }
        });
    }
});

function getExportSpecifierForDiagnosticSpan(span: TextSpan, sourceFile: SourceFile) {
    return tryCast(getTokenAtPosition(sourceFile, span.start).parent, isExportSpecifier);
}

/**
 * Fixes a single export declaration by adding a type-only export if necessary.
 * @param changes - The text changes to be made.
 * @param exportSpecifier - The export specifier to be fixed.
 * @param context - The code fix context.
 */
function fixSingleExportDeclaration(changes: textChanges.ChangeTracker, exportSpecifier: ExportSpecifier | undefined, context: CodeFixContextBase) {
    if (!exportSpecifier) {
        return;
    }

    const exportClause = exportSpecifier.parent;
    const exportDeclaration = exportClause.parent;
    const typeExportSpecifiers = getTypeExportSpecifiers(exportSpecifier, context);
    if (typeExportSpecifiers.length === exportClause.elements.length) {
        changes.insertModifierBefore(context.sourceFile, SyntaxKind.TypeKeyword, exportClause);
    }
    else {
        const valueExportDeclaration = factory.updateExportDeclaration(
            exportDeclaration,
            exportDeclaration.modifiers,
            /*isTypeOnly*/ false,
            factory.updateNamedExports(exportClause, filter(exportClause.elements, e => !contains(typeExportSpecifiers, e))),
            exportDeclaration.moduleSpecifier,
            /*assertClause*/ undefined
        );
        const typeExportDeclaration = factory.createExportDeclaration(
            /*modifiers*/ undefined,
            /*isTypeOnly*/ true,
            factory.createNamedExports(typeExportSpecifiers),
            exportDeclaration.moduleSpecifier,
            /*assertClause*/ undefined
        );

        changes.replaceNode(context.sourceFile, exportDeclaration, valueExportDeclaration, {
            leadingTriviaOption: textChanges.LeadingTriviaOption.IncludeAll,
            trailingTriviaOption: textChanges.TrailingTriviaOption.Exclude
        });
        changes.insertNodeAfter(context.sourceFile, exportDeclaration, typeExportDeclaration);
    }
}

/**
 * Returns an array of ExportSpecifier objects that match the specified originExportSpecifier and pass the provided context's semantic diagnostics.
 * @param originExportSpecifier - The ExportSpecifier object to match against.
 * @param context - The CodeFixContextBase object containing the program and source file to check for semantic diagnostics.
 * @returns An array of ExportSpecifier objects that match the specified originExportSpecifier and pass the provided context's semantic diagnostics.
 */
function getTypeExportSpecifiers(originExportSpecifier: ExportSpecifier, context: CodeFixContextBase): readonly ExportSpecifier[] {
    const exportClause = originExportSpecifier.parent;
    if (exportClause.elements.length === 1) {
        return exportClause.elements;
    }

    const diagnostics = getDiagnosticsWithinSpan(
        createTextSpanFromNode(exportClause),
        context.program.getSemanticDiagnostics(context.sourceFile, context.cancellationToken));

    return filter(exportClause.elements, element => {
        return element === originExportSpecifier || findDiagnosticForNode(element, diagnostics)?.code === errorCodes[0];
    });
}
