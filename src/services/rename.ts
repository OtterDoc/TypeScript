import {
    compareStringsCaseSensitive,
    Comparison,
    createTextSpan,
    DiagnosticMessage,
    Diagnostics,
    endsWith,
    every,
    Extension,
    fileExtensionIs,
    find,
    getAdjustedRenameLocation,
    getContextualTypeFromParentOrAncestorTypeNode,
    getLocaleSpecificMessage,
    getPathComponents,
    getTextOfIdentifierOrLiteral,
    getTextOfNode,
    getTouchingPropertyName,
    ImportSpecifier,
    isExternalModuleNameRelative,
    isIdentifier,
    isImportOrExportSpecifierName,
    isImportSpecifier,
    isInsideNodeModules,
    isLabelName,
    isLiteralNameOfPropertyDeclarationOrIndexAccess,
    isSourceFile,
    isStringLiteralLike,
    isStringOrNumericLiteralLike,
    Node,
    NumericLiteral,
    Path,
    Program,
    removeFileExtension,
    RenameInfo,
    RenameInfoFailure,
    RenameInfoSuccess,
    ScriptElementKind,
    ScriptElementKindModifier,
    some,
    SourceFile,
    StringLiteralLike,
    stripQuotes,
    Symbol,
    SymbolDisplay,
    SymbolFlags,
    SyntaxKind,
    tryGetImportFromModuleSpecifier,
    tryRemoveSuffix,
    TypeChecker,
    TypeFlags,
    UnionType,
    UserPreferences,
} from "./_namespaces/ts";

/**
 * Retrieves rename information for a given position in a source file.
 * @param program - The TypeScript program.
 * @param sourceFile - The source file containing the position.
 * @param position - The position to retrieve rename information for.
 * @param preferences - The user preferences for renaming.
 * @returns The rename information for the given position, or an error message if renaming is not possible.
 * @remarks This function is intended for internal use only.
 */
export function getRenameInfo(program: Program, sourceFile: SourceFile, position: number, preferences: UserPreferences): RenameInfo {
    const node = getAdjustedRenameLocation(getTouchingPropertyName(sourceFile, position));
    if (nodeIsEligibleForRename(node)) {
        const renameInfo = getRenameInfoForNode(node, program.getTypeChecker(), sourceFile, program, preferences);
        if (renameInfo) {
            return renameInfo;
        }
    }
    return getRenameInfoError(Diagnostics.You_cannot_rename_this_element);
}

/**
 * Returns information necessary for renaming a node in a TypeScript program.
 * @param node The node to be renamed.
 * @param typeChecker The TypeChecker instance for the program.
 * @param sourceFile The SourceFile containing the node.
 * @param program The Program instance for the program.
 * @param preferences User preferences for the program.
 * @returns RenameInfo object if renaming is allowed, undefined otherwise.
 */
function getRenameInfoForNode(
    node: Node,
    typeChecker: TypeChecker,
    sourceFile: SourceFile,
    program: Program,
    preferences: UserPreferences): RenameInfo | undefined {
    const symbol = typeChecker.getSymbolAtLocation(node);
    if (!symbol) {
        if (isStringLiteralLike(node)) {
            const type = getContextualTypeFromParentOrAncestorTypeNode(node, typeChecker);
            if (type && ((type.flags & TypeFlags.StringLiteral) || (
                (type.flags & TypeFlags.Union) && every((type as UnionType).types, type => !!(type.flags & TypeFlags.StringLiteral))
            ))) {
                return getRenameInfoSuccess(node.text, node.text, ScriptElementKind.string, "", node, sourceFile);
            }
        }
        else if (isLabelName(node)) {
            const name = getTextOfNode(node);
            return getRenameInfoSuccess(name, name, ScriptElementKind.label, ScriptElementKindModifier.none, node, sourceFile);
        }
        return undefined;
    }
    // Only allow a symbol to be renamed if it actually has at least one declaration.
    const { declarations } = symbol;
    if (!declarations || declarations.length === 0) return;

    // Disallow rename for elements that are defined in the standard TypeScript library.
    if (declarations.some(declaration => isDefinedInLibraryFile(program, declaration))) {
        return getRenameInfoError(Diagnostics.You_cannot_rename_elements_that_are_defined_in_the_standard_TypeScript_library);
    }

    // Cannot rename `default` as in `import { default as foo } from "./someModule";
    if (isIdentifier(node) && node.escapedText === "default" && symbol.parent && symbol.parent.flags & SymbolFlags.Module) {
        return undefined;
    }

    if (isStringLiteralLike(node) && tryGetImportFromModuleSpecifier(node)) {
        return preferences.allowRenameOfImportPath ? getRenameInfoForModule(node, sourceFile, symbol) : undefined;
    }

    // Disallow rename for elements that would rename across `*/node_modules/*` packages.
    const wouldRenameNodeModules = wouldRenameInOtherNodeModules(sourceFile, symbol, typeChecker, preferences);
    if (wouldRenameNodeModules) {
        return getRenameInfoError(wouldRenameNodeModules);
    }

    const kind = SymbolDisplay.getSymbolKind(typeChecker, symbol, node);
    const specifierName = (isImportOrExportSpecifierName(node) || isStringOrNumericLiteralLike(node) && node.parent.kind === SyntaxKind.ComputedPropertyName)
        ? stripQuotes(getTextOfIdentifierOrLiteral(node))
        : undefined;
    const displayName = specifierName || typeChecker.symbolToString(symbol);
    const fullDisplayName = specifierName || typeChecker.getFullyQualifiedName(symbol);
    return getRenameInfoSuccess(displayName, fullDisplayName, kind, SymbolDisplay.getSymbolModifiers(typeChecker,symbol), node, sourceFile);
}

function isDefinedInLibraryFile(program: Program, declaration: Node) {
    const sourceFile = declaration.getSourceFile();
    return program.isSourceFileDefaultLibrary(sourceFile) && fileExtensionIs(sourceFile.fileName, Extension.Dts);
}

/**
 * Checks if renaming a symbol defined in a node_modules folder would cause conflicts with other packages.
 * @param {SourceFile} originalFile - The original source file.
 * @param {Symbol} symbol - The symbol to be renamed.
 * @param {TypeChecker} checker - The type checker.
 * @param {UserPreferences} preferences - The user preferences.
 * @returns {DiagnosticMessage | undefined} - A diagnostic message if renaming the symbol would cause conflicts, otherwise undefined.
 */
function wouldRenameInOtherNodeModules(
    originalFile: SourceFile,
    symbol: Symbol,
    checker: TypeChecker,
    preferences: UserPreferences
): DiagnosticMessage | undefined {
    if (!preferences.providePrefixAndSuffixTextForRename && symbol.flags & SymbolFlags.Alias) {
        const importSpecifier = symbol.declarations && find(symbol.declarations, decl => isImportSpecifier(decl));
        if (importSpecifier && !(importSpecifier as ImportSpecifier).propertyName) {
            symbol = checker.getAliasedSymbol(symbol);
        }
    }
    const { declarations } = symbol;
    if (!declarations) {
        return undefined;
    }
    const originalPackage = getPackagePathComponents(originalFile.path);
    if (originalPackage === undefined) { // original source file is not in node_modules
        if (some(declarations, declaration => isInsideNodeModules(declaration.getSourceFile().path))) {
            return Diagnostics.You_cannot_rename_elements_that_are_defined_in_a_node_modules_folder;
        }
        else {
            return undefined;
        }
    }
    // original source file is in node_modules
    for (const declaration of declarations) {
        const declPackage = getPackagePathComponents(declaration.getSourceFile().path);
        if (declPackage) {
            const length = Math.min(originalPackage.length, declPackage.length);
            for (let i = 0; i <= length; i++) {
                if (compareStringsCaseSensitive(originalPackage[i], declPackage[i]) !== Comparison.EqualTo) {
                    return Diagnostics.You_cannot_rename_elements_that_are_defined_in_another_node_modules_folder;
                }
            }
        }
    }
    return undefined;
}

function getPackagePathComponents(filePath: Path): string[] | undefined {
    const components = getPathComponents(filePath) as string[];
    const nodeModulesIdx = components.lastIndexOf("node_modules");
    if (nodeModulesIdx === -1) {
        return undefined;
    }
    return components.slice(0, nodeModulesIdx + 2);
}

/**
 * Returns information necessary for renaming a module.
 * @param node - The StringLiteralLike node representing the module to be renamed.
 * @param sourceFile - The SourceFile containing the module to be renamed.
 * @param moduleSymbol - The Symbol representing the module to be renamed.
 * @returns Either a RenameInfo object or undefined if the module cannot be renamed.
 */
function getRenameInfoForModule(node: StringLiteralLike, sourceFile: SourceFile, moduleSymbol: Symbol): RenameInfo | undefined {
    if (!isExternalModuleNameRelative(node.text)) {
        return getRenameInfoError(Diagnostics.You_cannot_rename_a_module_via_a_global_import);
    }

    const moduleSourceFile = moduleSymbol.declarations && find(moduleSymbol.declarations, isSourceFile);
    if (!moduleSourceFile) return undefined;
    const withoutIndex = endsWith(node.text, "/index") || endsWith(node.text, "/index.js") ? undefined : tryRemoveSuffix(removeFileExtension(moduleSourceFile.fileName), "/index");
    const name = withoutIndex === undefined ? moduleSourceFile.fileName : withoutIndex;
    const kind = withoutIndex === undefined ? ScriptElementKind.moduleElement : ScriptElementKind.directory;
    const indexAfterLastSlash = node.text.lastIndexOf("/") + 1;
    // Span should only be the last component of the path. + 1 to account for the quote character.
    const triggerSpan = createTextSpan(node.getStart(sourceFile) + 1 + indexAfterLastSlash, node.text.length - indexAfterLastSlash);
    return {
        canRename: true,
        fileToRename: name,
        kind,
        displayName: name,
        fullDisplayName: name,
        kindModifiers: ScriptElementKindModifier.none,
        triggerSpan,
    };
}

function getRenameInfoSuccess(displayName: string, fullDisplayName: string, kind: ScriptElementKind, kindModifiers: string, node: Node, sourceFile: SourceFile): RenameInfoSuccess {
    return {
        canRename: true,
        fileToRename: undefined,
        kind,
        displayName,
        fullDisplayName,
        kindModifiers,
        triggerSpan: createTriggerSpanForNode(node, sourceFile)
    };
}

function getRenameInfoError(diagnostic: DiagnosticMessage): RenameInfoFailure {
    return { canRename: false, localizedErrorMessage: getLocaleSpecificMessage(diagnostic) };
}

function createTriggerSpanForNode(node: Node, sourceFile: SourceFile) {
    let start = node.getStart(sourceFile);
    let width = node.getWidth(sourceFile);
    if (isStringLiteralLike(node)) {
        // Exclude the quotes
        start += 1;
        width -= 2;
    }
    return createTextSpan(start, width);
}

/** @internal */
export function nodeIsEligibleForRename(node: Node): boolean {
    switch (node.kind) {
        case SyntaxKind.Identifier:
        case SyntaxKind.PrivateIdentifier:
        case SyntaxKind.StringLiteral:
        case SyntaxKind.NoSubstitutionTemplateLiteral:
        case SyntaxKind.ThisKeyword:
            return true;
        case SyntaxKind.NumericLiteral:
            return isLiteralNameOfPropertyDeclarationOrIndexAccess(node as NumericLiteral);
        default:
            return false;
    }
}
