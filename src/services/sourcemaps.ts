import {
    base64decode,
    computeLineAndCharacterOfPosition,
    createDocumentPositionMapper,
    createGetCanonicalFileName,
    DocumentPosition,
    DocumentPositionMapper,
    DocumentPositionMapperHost,
    Extension,
    getDeclarationEmitOutputFilePathWorker,
    getDirectoryPath,
    getDocumentPositionMapper as ts_getDocumentPositionMapper,
    getLineInfo,
    getLineStarts,
    getNormalizedAbsolutePath,
    identitySourceMapConsumer,
    isDeclarationFileName,
    isString,
    LineAndCharacter,
    LineInfo,
    outFile,
    Program,
    removeFileExtension,
    SourceFileLike,
    sys,
    toPath as ts_toPath,
    tryGetSourceMappingURL,
    tryParseRawSourceMap,
} from "./_namespaces/ts";

const base64UrlRegExp = /^data:(?:application\/json(?:;charset=[uU][tT][fF]-8);base64,([A-Za-z0-9+\/=]+)$)?/;

/** @internal */
export interface SourceMapper {
    toLineColumnOffset(fileName: string, position: number): LineAndCharacter;
    tryGetSourcePosition(info: DocumentPosition): DocumentPosition | undefined;
    tryGetGeneratedPosition(info: DocumentPosition): DocumentPosition | undefined;
    clearCache(): void;
}

/**
 * Interface for a host that provides information needed to map generated code back to its original source.
 * @internal
 */
export interface SourceMapperHost {
    useCaseSensitiveFileNames(): boolean;
    getCurrentDirectory(): string;
    getProgram(): Program | undefined;
    fileExists?(path: string): boolean;
    readFile?(path: string, encoding?: string): string | undefined;
    getSourceFileLike?(fileName: string): SourceFileLike | undefined;
    getDocumentPositionMapper?(generatedFileName: string, sourceFileName?: string): DocumentPositionMapper | undefined;
    log(s: string): void;
}

/** @internal */
export function getSourceMapper(host: SourceMapperHost): SourceMapper {
    const getCanonicalFileName = createGetCanonicalFileName(host.useCaseSensitiveFileNames());
    const currentDirectory = host.getCurrentDirectory();
    const sourceFileLike = new Map<string, SourceFileLike | false>();
    const documentPositionMappers = new Map<string, DocumentPositionMapper>();
    return { tryGetSourcePosition, tryGetGeneratedPosition, toLineColumnOffset, clearCache };

    function toPath(fileName: string) {
        return ts_toPath(fileName, currentDirectory, getCanonicalFileName);
    }

    /**
     * Returns the DocumentPositionMapper for the given generatedFileName and sourceFileName.
     * @param {string} generatedFileName - The name of the generated file.
     * @param {string} [sourceFileName] - The name of the source file.
     * @returns {DocumentPositionMapper} The DocumentPositionMapper for the given files.
     */
    function getDocumentPositionMapper(generatedFileName: string, sourceFileName?: string) {
        const path = toPath(generatedFileName);
        const value = documentPositionMappers.get(path);
        if (value) return value;

        let mapper: DocumentPositionMapper | undefined;
        if (host.getDocumentPositionMapper) {
            mapper = host.getDocumentPositionMapper(generatedFileName, sourceFileName);
        }
        else if (host.readFile) {
            const file = getSourceFileLike(generatedFileName);
            mapper = file && ts_getDocumentPositionMapper(
                { getSourceFileLike, getCanonicalFileName, log: s => host.log(s) },
                generatedFileName,
                getLineInfo(file.text, getLineStarts(file)),
                f => !host.fileExists || host.fileExists(f) ? host.readFile!(f) : undefined
            );
        }
        documentPositionMappers.set(path, mapper || identitySourceMapConsumer);
        return mapper || identitySourceMapConsumer;
    }

    function tryGetSourcePosition(info: DocumentPosition): DocumentPosition | undefined {
        if (!isDeclarationFileName(info.fileName)) return undefined;

        const file = getSourceFile(info.fileName);
        if (!file) return undefined;

        const newLoc = getDocumentPositionMapper(info.fileName).getSourcePosition(info);
        return !newLoc || newLoc === info ? undefined : tryGetSourcePosition(newLoc) || newLoc;
    }

    /**
     * Tries to get the generated position of a given DocumentPosition.
     * @param {DocumentPosition} info - The DocumentPosition to get the generated position for.
     * @returns {DocumentPosition | undefined} - The generated position or undefined if it cannot be found.
     */
    function tryGetGeneratedPosition(info: DocumentPosition): DocumentPosition | undefined {
        if (isDeclarationFileName(info.fileName)) return undefined;

        const sourceFile = getSourceFile(info.fileName);
        if (!sourceFile) return undefined;

        const program = host.getProgram()!;
        // If this is source file of project reference source (instead of redirect) there is no generated position
        if (program.isSourceOfProjectReferenceRedirect(sourceFile.fileName)) {
            return undefined;
        }

        const options = program.getCompilerOptions();
        const outPath = outFile(options);

        const declarationPath = outPath ?
            removeFileExtension(outPath) + Extension.Dts :
            getDeclarationEmitOutputFilePathWorker(info.fileName, program.getCompilerOptions(), currentDirectory, program.getCommonSourceDirectory(), getCanonicalFileName);
        if (declarationPath === undefined) return undefined;

        const newLoc = getDocumentPositionMapper(declarationPath, info.fileName).getGeneratedPosition(info);
        return newLoc === info ? undefined : newLoc;
    }

    function getSourceFile(fileName: string) {
        const program = host.getProgram();
        if (!program) return undefined;

        const path = toPath(fileName);
        // file returned here could be .d.ts when asked for .ts file if projectReferences and module resolution created this source file
        const file = program.getSourceFileByPath(path);
        return file && file.resolvedPath === path ? file : undefined;
    }

    /**
     * Retrieves a SourceFileLike object for the given file name, creating one if it doesn't exist.
     * @param {string} fileName - The name of the file to retrieve or create.
     * @returns {SourceFileLike | undefined} - The retrieved or created SourceFileLike object, or undefined if it could not be created.
     */
    function getOrCreateSourceFileLike(fileName: string): SourceFileLike | undefined {
        const path = toPath(fileName);
        const fileFromCache = sourceFileLike.get(path);
        if (fileFromCache !== undefined) return fileFromCache ? fileFromCache : undefined;

        if (!host.readFile || host.fileExists && !host.fileExists(path)) {
            sourceFileLike.set(path, false);
            return undefined;
        }

        // And failing that, check the disk
        const text = host.readFile(path);
        const file = text ? createSourceFileLike(text) : false;
        sourceFileLike.set(path, file);
        return file ? file : undefined;
    }

    // This can be called from source mapper in either source program or program that includes generated file
    function getSourceFileLike(fileName: string) {
        return !host.getSourceFileLike ?
            getSourceFile(fileName) || getOrCreateSourceFileLike(fileName) :
            host.getSourceFileLike(fileName);
    }

    function toLineColumnOffset(fileName: string, position: number): LineAndCharacter {
        const file = getSourceFileLike(fileName)!; // TODO: GH#18217
        return file.getLineAndCharacterOfPosition(position);
    }

    function clearCache(): void {
        sourceFileLike.clear();
        documentPositionMappers.clear();
    }
}

/**
 * string | undefined to contents of map file to create DocumentPositionMapper from it
 * DocumentPositionMapper | false to give back cached DocumentPositionMapper
 *
 * @internal
 */
export type ReadMapFile = (mapFileName: string, mapFileNameFromDts: string | undefined) => string | undefined | DocumentPositionMapper | false;

/**
 * Returns a DocumentPositionMapper function based on the provided parameters.
 * @param {DocumentPositionMapperHost} host - The host object for the DocumentPositionMapper.
 * @param {string} generatedFileName - The name of the generated file.
 * @param {LineInfo} generatedFileLineInfo - The line information for the generated file.
 * @param {ReadMapFile} readMapFile - The function used to read the map file.
 * @returns {DocumentPositionMapper | undefined} - The DocumentPositionMapper function or undefined if it cannot be created.
 * @remarks This function first attempts to get the map file name from the generated file line information. If it is a data URL, it decodes the base64 object and converts it to a source mapper. If it is not a data URL, it attempts to read the map file from possible locations. If the map file contents are a string, it converts it to a source mapper. If the map file contents are undefined, it returns undefined. If none of the attempts are successful, it returns undefined.
 */
export function getDocumentPositionMapper(
    host: DocumentPositionMapperHost,
    generatedFileName: string,
    generatedFileLineInfo: LineInfo,
    readMapFile: ReadMapFile) {
    let mapFileName = tryGetSourceMappingURL(generatedFileLineInfo);
    if (mapFileName) {
        const match = base64UrlRegExp.exec(mapFileName);
        if (match) {
            if (match[1]) {
                const base64Object = match[1];
                return convertDocumentToSourceMapper(host, base64decode(sys, base64Object), generatedFileName);
            }
            // Not a data URL we can parse, skip it
            mapFileName = undefined;
        }
    }
    const possibleMapLocations: string[] = [];
    if (mapFileName) {
        possibleMapLocations.push(mapFileName);
    }
    possibleMapLocations.push(generatedFileName + ".map");
    const originalMapFileName = mapFileName && getNormalizedAbsolutePath(mapFileName, getDirectoryPath(generatedFileName));
    for (const location of possibleMapLocations) {
        const mapFileName = getNormalizedAbsolutePath(location, getDirectoryPath(generatedFileName));
        const mapFileContents = readMapFile(mapFileName, originalMapFileName);
        if (isString(mapFileContents)) {
            return convertDocumentToSourceMapper(host, mapFileContents, mapFileName);
        }
        if (mapFileContents !== undefined) {
            return mapFileContents || undefined;
        }
    }
    return undefined;
}

function convertDocumentToSourceMapper(host: DocumentPositionMapperHost, contents: string, mapFileName: string) {
    const map = tryParseRawSourceMap(contents);
    if (!map || !map.sources || !map.file || !map.mappings) {
        // obviously invalid map
        return undefined;
    }

    // Dont support sourcemaps that contain inlined sources
    if (map.sourcesContent && map.sourcesContent.some(isString)) return undefined;

    return createDocumentPositionMapper(host, map, mapFileName);
}

function createSourceFileLike(text: string, lineMap?: SourceFileLike["lineMap"]): SourceFileLike {
    return {
        text,
        lineMap,
        getLineAndCharacterOfPosition(pos: number) {
            return computeLineAndCharacterOfPosition(getLineStarts(this), pos);
        }
    };
}
