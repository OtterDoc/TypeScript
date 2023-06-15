import {
    combinePaths,
    createGetCanonicalFileName,
    createModuleSpecifierResolutionHost,
    createRange,
    Debug,
    emptyArray,
    endsWith,
    ensurePathIsNonModuleName,
    Expression,
    factory,
    FileTextChanges,
    find,
    forEach,
    formatting,
    GetCanonicalFileName,
    getDirectoryPath,
    getFileMatcherPatterns,
    getModeForUsageLocation,
    getOptionFromName,
    getRegexFromPattern,
    getRelativePathFromDirectory,
    getRelativePathFromFile,
    getTsConfigObjectLiteralExpression,
    hostUsesCaseSensitiveFileNames,
    isAmbientModule,
    isArrayLiteralExpression,
    isObjectLiteralExpression,
    isPropertyAssignment,
    isSourceFile,
    isStringLiteral,
    LanguageServiceHost,
    last,
    mapDefined,
    ModuleResolutionHost,
    moduleSpecifiers,
    normalizePath,
    Path,
    pathIsRelative,
    Program,
    PropertyAssignment,
    ResolvedModuleWithFailedLookupLocations,
    resolveModuleName,
    SourceFile,
    SourceFileLike,
    SourceMapper,
    StringLiteralLike,
    Symbol,
    textChanges,
    TextRange,
    tryRemoveDirectoryPrefix,
    UserPreferences,
} from "./_namespaces/ts";

/**
 * Returns an array of FileTextChanges for renaming a file or directory within a TypeScript program.
 * @param program - The TypeScript program.
 * @param oldFileOrDirPath - The old file or directory path.
 * @param newFileOrDirPath - The new file or directory path.
 * @param host - The LanguageServiceHost.
 * @param formatContext - The FormatContext.
 * @param preferences - The UserPreferences.
 * @param sourceMapper - The SourceMapper.
 * @returns An array of FileTextChanges.
 * @internal
 */
export function getEditsForFileRename(
    program: Program,
    oldFileOrDirPath: string,
    newFileOrDirPath: string,
    host: LanguageServiceHost,
    formatContext: formatting.FormatContext,
    preferences: UserPreferences,
    sourceMapper: SourceMapper,
): readonly FileTextChanges[] {
    const useCaseSensitiveFileNames = hostUsesCaseSensitiveFileNames(host);
    const getCanonicalFileName = createGetCanonicalFileName(useCaseSensitiveFileNames);
    const oldToNew = getPathUpdater(oldFileOrDirPath, newFileOrDirPath, getCanonicalFileName, sourceMapper);
    const newToOld = getPathUpdater(newFileOrDirPath, oldFileOrDirPath, getCanonicalFileName, sourceMapper);
    return textChanges.ChangeTracker.with({ host, formatContext, preferences }, changeTracker => {
        updateTsconfigFiles(program, changeTracker, oldToNew, oldFileOrDirPath, newFileOrDirPath, host.getCurrentDirectory(), useCaseSensitiveFileNames);
        updateImports(program, changeTracker, oldToNew, newToOld, host, getCanonicalFileName);
    });
}

/**
 * If 'path' refers to an old directory, returns path in the new directory.
 *
 * @internal
 */
export type PathUpdater = (path: string) => string | undefined;
// exported for tests
/**
 * Returns a PathUpdater function that updates paths based on the provided old and new file or directory paths.
 * @param {string} oldFileOrDirPath - The old file or directory path to update.
 * @param {string} newFileOrDirPath - The new file or directory path to update to.
 * @param {GetCanonicalFileName} getCanonicalFileName - A function that returns the canonical form of a file name.
 * @param {SourceMapper | undefined} sourceMapper - An optional source mapper object.
 * @returns {PathUpdater} A function that updates paths based on the provided old and new file or directory paths.
 * @remarks This function is marked as internal and should not be used outside of its module.
 */
export function getPathUpdater(oldFileOrDirPath: string, newFileOrDirPath: string, getCanonicalFileName: GetCanonicalFileName, sourceMapper: SourceMapper | undefined): PathUpdater {
    const canonicalOldPath = getCanonicalFileName(oldFileOrDirPath);
    return path => {
        const originalPath = sourceMapper && sourceMapper.tryGetSourcePosition({ fileName: path, pos: 0 });
        const updatedPath = getUpdatedPath(originalPath ? originalPath.fileName : path);
        return originalPath
            ? updatedPath === undefined ? undefined : makeCorrespondingRelativeChange(originalPath.fileName, updatedPath, path, getCanonicalFileName)
            : updatedPath;
    };

    function getUpdatedPath(pathToUpdate: string): string | undefined {
        if (getCanonicalFileName(pathToUpdate) === canonicalOldPath) return newFileOrDirPath;
        const suffix = tryRemoveDirectoryPrefix(pathToUpdate, canonicalOldPath, getCanonicalFileName);
        return suffix === undefined ? undefined : newFileOrDirPath + "/" + suffix;
    }
}

// Relative path from a0 to b0 should be same as relative path from a1 to b1. Returns b1.
function makeCorrespondingRelativeChange(a0: string, b0: string, a1: string, getCanonicalFileName: GetCanonicalFileName): string {
    const rel = getRelativePathFromFile(a0, b0, getCanonicalFileName);
    return combinePathsSafe(getDirectoryPath(a1), rel);
}

/**
 * Updates the paths in the tsconfig files based on the provided parameters.
 * @param program - The program to update the tsconfig files for.
 * @param changeTracker - The change tracker to use for updating the files.
 * @param oldToNew - The path updater to use for updating the paths.
 * @param oldFileOrDirPath - The old file or directory path to update.
 * @param newFileOrDirPath - The new file or directory path to update to.
 * @param currentDirectory - The current directory.
 * @param useCaseSensitiveFileNames - Whether or not to use case sensitive file names.
 */
function updateTsconfigFiles(program: Program, changeTracker: textChanges.ChangeTracker, oldToNew: PathUpdater, oldFileOrDirPath: string, newFileOrDirPath: string, currentDirectory: string, useCaseSensitiveFileNames: boolean): void {
    const { configFile } = program.getCompilerOptions();
    if (!configFile) return;
    const configDir = getDirectoryPath(configFile.fileName);

    const jsonObjectLiteral = getTsConfigObjectLiteralExpression(configFile);
    if (!jsonObjectLiteral) return;

    forEachProperty(jsonObjectLiteral, (property, propertyName) => {
        switch (propertyName) {
            case "files":
            case "include":
            case "exclude": {
                const foundExactMatch = updatePaths(property);
                if (foundExactMatch || propertyName !== "include" || !isArrayLiteralExpression(property.initializer)) return;
                const includes = mapDefined(property.initializer.elements, e => isStringLiteral(e) ? e.text : undefined);
                if (includes.length === 0) return;
                const matchers = getFileMatcherPatterns(configDir, /*excludes*/[], includes, useCaseSensitiveFileNames, currentDirectory);
                // If there isn't some include for this, add a new one.
                if (getRegexFromPattern(Debug.checkDefined(matchers.includeFilePattern), useCaseSensitiveFileNames).test(oldFileOrDirPath) &&
                    !getRegexFromPattern(Debug.checkDefined(matchers.includeFilePattern), useCaseSensitiveFileNames).test(newFileOrDirPath)) {
                    changeTracker.insertNodeAfter(configFile, last(property.initializer.elements), factory.createStringLiteral(relativePath(newFileOrDirPath)));
                }
                return;
            }
            case "compilerOptions":
                forEachProperty(property.initializer, (property, propertyName) => {
                    const option = getOptionFromName(propertyName);
                    Debug.assert(option?.type !== "listOrElement");
                    if (option && (option.isFilePath || option.type === "list" && option.element.isFilePath)) {
                        updatePaths(property);
                    }
                    else if (propertyName === "paths") {
                        forEachProperty(property.initializer, (pathsProperty) => {
                            if (!isArrayLiteralExpression(pathsProperty.initializer)) return;
                            for (const e of pathsProperty.initializer.elements) {
                                tryUpdateString(e);
                            }
                        });
                    }
                });
                return;
        }
    });

    function updatePaths(property: PropertyAssignment): boolean {
        const elements = isArrayLiteralExpression(property.initializer) ? property.initializer.elements : [property.initializer];
        let foundExactMatch = false;
        for (const element of elements) {
            foundExactMatch = tryUpdateString(element) || foundExactMatch;
        }
        return foundExactMatch;
    }

    /**
     * Tries to update a string element with a new path.
     * @param {Expression} element - The element to update.
     * @returns {boolean} - True if the element was updated, false otherwise.
     */
    function tryUpdateString(element: Expression): boolean {
        if (!isStringLiteral(element)) return false;
        const elementFileName = combinePathsSafe(configDir, element.text);

        const updated = oldToNew(elementFileName);
        if (updated !== undefined) {
            changeTracker.replaceRangeWithText(configFile!, createStringRange(element, configFile!), relativePath(updated));
            return true;
        }
        return false;
    }

    function relativePath(path: string): string {
        return getRelativePathFromDirectory(configDir, path, /*ignoreCase*/ !useCaseSensitiveFileNames);
    }
}

/**
 * Updates import statements in all source files of a given program based on a set of path updates.
 * @param program - The program to update imports in.
 * @param changeTracker - The change tracker to use for updating the source files.
 * @param oldToNew - A function that maps old paths to new paths.
 * @param newToOld - A function that maps new paths to old paths.
 * @param host - The language service host to use for resolving modules.
 * @param getCanonicalFileName - A function that returns the canonical file name for a given file name.
 */
function updateImports(
    program: Program,
    changeTracker: textChanges.ChangeTracker,
    oldToNew: PathUpdater,
    newToOld: PathUpdater,
    host: LanguageServiceHost,
    getCanonicalFileName: GetCanonicalFileName,
): void {
    const allFiles = program.getSourceFiles();
    for (const sourceFile of allFiles) {
        const newFromOld = oldToNew(sourceFile.fileName);
        const newImportFromPath = newFromOld ?? sourceFile.fileName;
        const newImportFromDirectory = getDirectoryPath(newImportFromPath);

        const oldFromNew: string | undefined = newToOld(sourceFile.fileName);
        const oldImportFromPath: string = oldFromNew || sourceFile.fileName;
        const oldImportFromDirectory = getDirectoryPath(oldImportFromPath);

        const importingSourceFileMoved = newFromOld !== undefined || oldFromNew !== undefined;

        updateImportsWorker(sourceFile, changeTracker,
            referenceText => {
                if (!pathIsRelative(referenceText)) return undefined;
                const oldAbsolute = combinePathsSafe(oldImportFromDirectory, referenceText);
                const newAbsolute = oldToNew(oldAbsolute);
                return newAbsolute === undefined ? undefined : ensurePathIsNonModuleName(getRelativePathFromDirectory(newImportFromDirectory, newAbsolute, getCanonicalFileName));
            },
            importLiteral => {
                const importedModuleSymbol = program.getTypeChecker().getSymbolAtLocation(importLiteral);
                // No need to update if it's an ambient module^M
                if (importedModuleSymbol?.declarations && importedModuleSymbol.declarations.some(d => isAmbientModule(d))) return undefined;

                const toImport = oldFromNew !== undefined
                    // If we're at the new location (file was already renamed), need to redo module resolution starting from the old location.
                    // TODO:GH#18217
                    ? getSourceFileToImportFromResolved(importLiteral, resolveModuleName(importLiteral.text, oldImportFromPath, program.getCompilerOptions(), host as ModuleResolutionHost),
                                                        oldToNew, allFiles)
                    : getSourceFileToImport(importedModuleSymbol, importLiteral, sourceFile, program, host, oldToNew);

                // Need an update if the imported file moved, or the importing file moved and was using a relative path.
                return toImport !== undefined && (toImport.updated || (importingSourceFileMoved && pathIsRelative(importLiteral.text)))
                    ? moduleSpecifiers.updateModuleSpecifier(program.getCompilerOptions(), sourceFile, getCanonicalFileName(newImportFromPath) as Path, toImport.newFileName, createModuleSpecifierResolutionHost(program, host), importLiteral.text)
                    : undefined;
            });
    }
}

function combineNormal(pathA: string, pathB: string): string {
    return normalizePath(combinePaths(pathA, pathB));
}
function combinePathsSafe(pathA: string, pathB: string): string {
    return ensurePathIsNonModuleName(combineNormal(pathA, pathB));
}

interface ToImport {
    readonly newFileName: string;
    /** True if the imported file was renamed. */
    readonly updated: boolean;
}
/**
 * Returns the file to import from based on the provided parameters.
 * @param {Symbol | undefined} importedModuleSymbol - The imported module symbol.
 * @param {StringLiteralLike} importLiteral - The import literal.
 * @param {SourceFile} importingSourceFile - The importing source file.
 * @param {Program} program - The program.
 * @param {LanguageServiceHost} host - The language service host.
 * @param {PathUpdater} oldToNew - The path updater.
 * @returns {ToImport | undefined} The file to import or undefined if not found.
 */
function getSourceFileToImport(
    importedModuleSymbol: Symbol | undefined,
    importLiteral: StringLiteralLike,
    importingSourceFile: SourceFile,
    program: Program,
    host: LanguageServiceHost,
    oldToNew: PathUpdater,
): ToImport | undefined {
    if (importedModuleSymbol) {
        // `find` should succeed because we checked for ambient modules before calling this function.
        const oldFileName = find(importedModuleSymbol.declarations, isSourceFile)!.fileName;
        const newFileName = oldToNew(oldFileName);
        return newFileName === undefined ? { newFileName: oldFileName, updated: false } : { newFileName, updated: true };
    }
    else {
        const mode = getModeForUsageLocation(importingSourceFile, importLiteral);
        const resolved = host.resolveModuleNameLiterals || !host.resolveModuleNames ?
            importingSourceFile.resolvedModules?.get(importLiteral.text, mode) :
            host.getResolvedModuleWithFailedLookupLocationsFromCache && host.getResolvedModuleWithFailedLookupLocationsFromCache(importLiteral.text, importingSourceFile.fileName, mode);
        return getSourceFileToImportFromResolved(importLiteral, resolved, oldToNew, program.getSourceFiles());
    }
}

/**
 * Returns an object containing the new file name and whether it was updated, if the importLiteral can be resolved to a new file location.
 * @param importLiteral - The import literal to resolve.
 * @param resolved - The resolved module with failed lookup locations.
 * @param oldToNew - The function to update the old file name to the new file name.
 * @param sourceFiles - An array of source files to search through.
 * @returns An object containing the new file name and whether it was updated, if the importLiteral can be resolved to a new file location. Otherwise, undefined.
 */
function getSourceFileToImportFromResolved(importLiteral: StringLiteralLike, resolved: ResolvedModuleWithFailedLookupLocations | undefined, oldToNew: PathUpdater, sourceFiles: readonly SourceFile[]): ToImport | undefined {
    // Search through all locations looking for a moved file, and only then test already existing files.
    // This is because if `a.ts` is compiled to `a.js` and `a.ts` is moved, we don't want to resolve anything to `a.js`, but to `a.ts`'s new location.
    if (!resolved) return undefined;

    // First try resolved module
    if (resolved.resolvedModule) {
        const result = tryChange(resolved.resolvedModule.resolvedFileName);
        if (result) return result;
    }

    // Then failed lookups that are in the list of sources
    const result = forEach(resolved.failedLookupLocations, tryChangeWithIgnoringPackageJsonExisting)
        // Then failed lookups except package.json since we dont want to touch them (only included ts/js files).
        // At this point, the confidence level of this fix being correct is too low to change bare specifiers or absolute paths.
        || pathIsRelative(importLiteral.text) && forEach(resolved.failedLookupLocations, tryChangeWithIgnoringPackageJson);
    if (result) return result;

    // If nothing changed, then result is resolved module file thats not updated
    return resolved.resolvedModule && { newFileName: resolved.resolvedModule.resolvedFileName, updated: false };

    function tryChangeWithIgnoringPackageJsonExisting(oldFileName: string) {
        const newFileName = oldToNew(oldFileName);
        return newFileName && find(sourceFiles, src => src.fileName === newFileName)
            ? tryChangeWithIgnoringPackageJson(oldFileName) : undefined;
    }

    function tryChangeWithIgnoringPackageJson(oldFileName: string) {
        return !endsWith(oldFileName, "/package.json") ? tryChange(oldFileName) : undefined;
    }

    function tryChange(oldFileName: string) {
        const newFileName = oldToNew(oldFileName);
        return newFileName && { newFileName, updated: true };
    }
}

/**
 * Updates the imports in a given source file by calling the provided updateRef and updateImport functions.
 * @param {SourceFile} sourceFile - The source file to update imports in.
 * @param {textChanges.ChangeTracker} changeTracker - The change tracker to use for making updates.
 * @param {(refText: string) => string | undefined} updateRef - The function to use for updating reference text.
 * @param {(importLiteral: StringLiteralLike) => string | undefined} updateImport - The function to use for updating import literals.
 */
function updateImportsWorker(sourceFile: SourceFile, changeTracker: textChanges.ChangeTracker, updateRef: (refText: string) => string | undefined, updateImport: (importLiteral: StringLiteralLike) => string | undefined) {
    for (const ref of sourceFile.referencedFiles || emptyArray) { // TODO: GH#26162
        const updated = updateRef(ref.fileName);
        if (updated !== undefined && updated !== sourceFile.text.slice(ref.pos, ref.end)) changeTracker.replaceRangeWithText(sourceFile, ref, updated);
    }

    for (const importStringLiteral of sourceFile.imports) {
        const updated = updateImport(importStringLiteral);
        if (updated !== undefined && updated !== importStringLiteral.text) changeTracker.replaceRangeWithText(sourceFile, createStringRange(importStringLiteral, sourceFile), updated);
    }
}

function createStringRange(node: StringLiteralLike, sourceFile: SourceFileLike): TextRange {
    return createRange(node.getStart(sourceFile) + 1, node.end - 1);
}

function forEachProperty(objectLiteral: Expression, cb: (property: PropertyAssignment, propertyName: string) => void) {
    if (!isObjectLiteralExpression(objectLiteral)) return;
    for (const property of objectLiteral.properties) {
        if (isPropertyAssignment(property) && isStringLiteral(property.name)) {
            cb(property, property.name.text);
        }
    }
}
