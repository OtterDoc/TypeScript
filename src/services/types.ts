import {
    CancellationToken,
    CompilerHost,
    CompilerOptions,
    CustomTransformers,
    Diagnostic,
    DiagnosticWithLocation,
    DocumentHighlights,
    DocumentPositionMapper,
    EmitOutput,
    ExportInfoMap,
    FileReference,
    GetEffectiveTypeRootsHost,
    HasChangedAutomaticTypeDirectiveNames,
    HasInvalidatedResolutions,
    LineAndCharacter,
    MinimalResolutionCacheHost,
    ModuleResolutionCache,
    ModuleSpecifierCache,
    ParsedCommandLine,
    Path,
    Program,
    ProjectReference,
    ResolutionMode,
    ResolvedModule,
    ResolvedModuleWithFailedLookupLocations,
    ResolvedProjectReference,
    ResolvedTypeReferenceDirective,
    ResolvedTypeReferenceDirectiveWithFailedLookupLocations,
    ScriptKind,
    SourceFile,
    SourceFileLike,
    SourceMapper,
    StringLiteralLike,
    Symbol,
    SymlinkCache,
    TextChangeRange,
    textChanges,
    TextRange,
    TextSpan,
    UserPreferences,
} from "./_namespaces/ts";

declare module "../compiler/types" {
    // Module transform: converted from interface augmentation
    /**
     * Interface representing a Node in the TypeScript AST.
     * @returns {SourceFile} The source file that contains the Node.
     * @returns {number} The number of child nodes the Node has.
     * @returns {Node} The child Node at the specified index.
     * @returns {Node[]} An array of all child Nodes.
     * @returns {number} The starting position of the Node.
     * @returns {number} The full starting position of the Node.
     * @returns {number} The ending position of the Node.
     * @returns {number} The width of the Node.
     * @returns {number} The full width of the Node.
     * @returns {number} The width of the leading trivia of the Node.
     * @returns {string} The full text of the Node.
     * @returns {string} The text of the Node.
     * @returns {Node | undefined} The first token of the Node.
     * @returns {Node | undefined} The last token of the Node.
     * @param {function} cbNode - A callback function to be executed on each child Node.
     * @param {function} cbNodeArray - A callback function to be executed on each child Node array.
     */
    export interface Node {
        getSourceFile(): SourceFile;
        getChildCount(sourceFile?: SourceFile): number;
        getChildAt(index: number, sourceFile?: SourceFile): Node;
        getChildren(sourceFile?: SourceFile): Node[];
        /** @internal */
        getChildren(sourceFile?: SourceFileLike): Node[]; // eslint-disable-line @typescript-eslint/unified-signatures
        getStart(sourceFile?: SourceFile, includeJsDocComment?: boolean): number;
        /** @internal */
        getStart(sourceFile?: SourceFileLike, includeJsDocComment?: boolean): number; // eslint-disable-line @typescript-eslint/unified-signatures
        getFullStart(): number;
        getEnd(): number;
        getWidth(sourceFile?: SourceFileLike): number;
        getFullWidth(): number;
        getLeadingTriviaWidth(sourceFile?: SourceFile): number;
        getFullText(sourceFile?: SourceFile): string;
        getText(sourceFile?: SourceFile): string;
        getFirstToken(sourceFile?: SourceFile): Node | undefined;
        /** @internal */
        getFirstToken(sourceFile?: SourceFileLike): Node | undefined; // eslint-disable-line @typescript-eslint/unified-signatures
        getLastToken(sourceFile?: SourceFile): Node | undefined;
        /** @internal */
        getLastToken(sourceFile?: SourceFileLike): Node | undefined; // eslint-disable-line @typescript-eslint/unified-signatures
        // See ts.forEachChild for documentation.
        forEachChild<T>(cbNode: (node: Node) => T | undefined, cbNodeArray?: (nodes: NodeArray<Node>) => T | undefined): T | undefined;
    }
}

declare module "../compiler/types" {
    // Module transform: converted from interface augmentation
    export interface Identifier {
        readonly text: string;
    }
}

declare module "../compiler/types" {
    // Module transform: converted from interface augmentation
    export interface PrivateIdentifier {
        readonly text: string;
    }
}

declare module "../compiler/types" {
    // Module transform: converted from interface augmentation
    /**
     * Represents a symbol, which is a named entity in a TypeScript program.
     * @readonly
     * @property {string} name - The name of the symbol.
     * @returns {SymbolFlags} - The flags associated with the symbol.
     * @returns {__String} - The escaped name of the symbol.
     * @returns {string} - The name of the symbol.
     * @returns {Declaration[] | undefined} - The declarations associated with the symbol.
     * @param {TypeChecker | undefined} typeChecker - The type checker to use for getting the documentation comment.
     * @returns {SymbolDisplayPart[]} - The documentation comment associated with the symbol.
     * @remarks This interface also includes internal methods for getting contextual documentation comments and JSDoc tags.
     */
    export interface Symbol {
        readonly name: string;
        getFlags(): SymbolFlags;
        getEscapedName(): __String;
        getName(): string;
        getDeclarations(): Declaration[] | undefined;
        getDocumentationComment(typeChecker: TypeChecker | undefined): SymbolDisplayPart[];
        /** @internal */
        getContextualDocumentationComment(context: Node | undefined, checker: TypeChecker | undefined): SymbolDisplayPart[]
        getJsDocTags(checker?: TypeChecker): JSDocTagInfo[];
        /** @internal */
        getContextualJsDocTags(context: Node | undefined, checker: TypeChecker | undefined): JSDocTagInfo[];
    }
}

declare module "../compiler/types" {
    // Module transform: converted from interface augmentation
    /**
     * Interface representing a TypeScript type.
     * @returns {TypeFlags} The flags associated with the type.
     * @returns {Symbol | undefined} The symbol associated with the type, if any.
     * @returns {Symbol[]} An array of symbols representing the properties of the type.
     * @returns {Symbol | undefined} The symbol representing the specified property, if any.
     * @returns {Symbol[]} An array of symbols representing the apparent properties of the type.
     * @returns {readonly Signature[]} An array of signatures representing the call signatures of the type.
     * @returns {readonly Signature[]} An array of signatures representing the construct signatures of the type.
     * @returns {Type | undefined} The string index type associated with the type, if any.
     * @returns {Type | undefined} The number index type associated with the type, if any.
     * @returns {BaseType[] | undefined} An array of base types associated with the type, if any.
     * @returns {Type} The non-nullable version of the type.
     * @remarks This method is internal and should not be used directly.
     * @returns {Type} The non-optional version of the type.
     * @remarks This method is internal and should not be used directly.
     * @returns {boolean} True if the type is nullable, false otherwise.
     * @returns {Type | undefined} The constraint associated with the type, if any.
     * @returns {Type | undefined} The default type associated with the type, if any.
     * @returns {boolean} True if the type is a union type, false otherwise.
     * @returns {boolean} True if the type is an intersection type, false otherwise.
     * @returns {boolean} True if the type is a union or intersection type, false otherwise.
     * @returns {boolean} True if the type is a literal type, false otherwise.
     * @returns {boolean} True if the type is a string literal type, false otherwise.
     * @returns {boolean} True if the type is a number literal type, false otherwise.
     * @returns {boolean} True if the type is a type parameter, false otherwise.
     * @returns {boolean} True if the type is a class or interface, false otherwise.
     * @returns {boolean} True if the type is a class, false otherwise.
     * @returns {boolean} True if the type is an index type, false otherwise.
     */
    export interface Type {
        getFlags(): TypeFlags;
        getSymbol(): Symbol | undefined;
        getProperties(): Symbol[];
        getProperty(propertyName: string): Symbol | undefined;
        getApparentProperties(): Symbol[];
        getCallSignatures(): readonly Signature[];
        getConstructSignatures(): readonly Signature[];
        getStringIndexType(): Type | undefined;
        getNumberIndexType(): Type | undefined;
        getBaseTypes(): BaseType[] | undefined;
        getNonNullableType(): Type;
        /** @internal */ getNonOptionalType(): Type;
        /** @internal */ isNullableType(): boolean;
        getConstraint(): Type | undefined;
        getDefault(): Type | undefined;

        isUnion(): this is UnionType;
        isIntersection(): this is IntersectionType;
        isUnionOrIntersection(): this is UnionOrIntersectionType;
        isLiteral(): this is LiteralType;
        isStringLiteral(): this is StringLiteralType;
        isNumberLiteral(): this is NumberLiteralType;
        isTypeParameter(): this is TypeParameter;
        isClassOrInterface(): this is InterfaceType;
        isClass(): this is InterfaceType;
        isIndexType(): this is IndexType;
    }
}

declare module "../compiler/types" {
    // Module transform: converted from interface augmentation
    export interface TypeReference {
        typeArguments?: readonly Type[];
    }
}

declare module "../compiler/types" {
    // Module transform: converted from interface augmentation
    export interface Signature {
        getDeclaration(): SignatureDeclaration;
        getTypeParameters(): TypeParameter[] | undefined;
        getParameters(): Symbol[];
        getTypeParameterAtPosition(pos: number): Type;
        getReturnType(): Type;
        getDocumentationComment(typeChecker: TypeChecker | undefined): SymbolDisplayPart[];
        getJsDocTags(): JSDocTagInfo[];
    }
}

declare module "../compiler/types" {
    // Module transform: converted from interface augmentation
    /**
     * Represents a source file in the TypeScript language service.
     * @interface
     */
    export interface SourceFile {
        /** @internal */ version: string;
        /** @internal */ scriptSnapshot: IScriptSnapshot | undefined;
        /** @internal */ nameTable: Map<__String, number> | undefined;

        /** @internal */ getNamedDeclarations(): Map<string, readonly Declaration[]>;

        getLineAndCharacterOfPosition(pos: number): LineAndCharacter;
        getLineEndOfPosition(pos: number): number;
        getLineStarts(): readonly number[];
        getPositionOfLineAndCharacter(line: number, character: number): number;
        update(newText: string, textChangeRange: TextChangeRange): SourceFile;

        /** @internal */ sourceMapper?: DocumentPositionMapper;
    }
}

declare module "../compiler/types" {
    // Module transform: converted from interface augmentation
    export interface SourceFileLike {
        getLineAndCharacterOfPosition(pos: number): LineAndCharacter;
    }
}

declare module "../compiler/types" {
    // Module transform: converted from interface augmentation
    export interface SourceMapSource {
        getLineAndCharacterOfPosition(pos: number): LineAndCharacter;
    }
}

/**
 * Represents an immutable snapshot of a script at a specified time.
 * Once acquired, the snapshot is observably immutable. i.e. the same calls with the same parameters will return
 * the same values.
 * @interface
 */
// eslint-disable-next-line @typescript-eslint/naming-convention
export interface IScriptSnapshot {
    /** Gets a portion of the script snapshot specified by [start, end). */
    getText(start: number, end: number): string;

    /** Gets the length of this script snapshot. */
    getLength(): number;

    /**
     * Gets the TextChangeRange that describe how the text changed between this text and
     * an older version.  This information is used by the incremental parser to determine
     * what sections of the script need to be re-parsed.  'undefined' can be returned if the
     * change range cannot be determined.  However, in that case, incremental parsing will
     * not happen and the entire document will be re - parsed.
     */
    getChangeRange(oldSnapshot: IScriptSnapshot): TextChangeRange | undefined;

    /** Releases all resources held by this script snapshot */
    dispose?(): void;
}

export namespace ScriptSnapshot {
    /**
     * Class representing a string script snapshot.
     * @implements {IScriptSnapshot}
     */
    class StringScriptSnapshot implements IScriptSnapshot {

        constructor(private text: string) {
        }

        public getText(start: number, end: number): string {
            return start === 0 && end === this.text.length
                ? this.text
                : this.text.substring(start, end);
        }

        public getLength(): number {
            return this.text.length;
        }

        public getChangeRange(): TextChangeRange | undefined {
            // Text-based snapshots do not support incremental parsing. Return undefined
            // to signal that to the caller.
            return undefined;
        }
    }

    export function fromString(text: string): IScriptSnapshot {
        return new StringScriptSnapshot(text);
    }
}

export interface PreProcessedFileInfo {
    referencedFiles: FileReference[];
    typeReferenceDirectives: FileReference[];
    libReferenceDirectives: FileReference[];
    importedFiles: FileReference[];
    ambientExternalModules?: string[];
    isLibFile: boolean;
}

export interface HostCancellationToken {
    isCancellationRequested(): boolean;
}

export interface InstallPackageOptions {
    fileName: Path;
    packageName: string;
}

/** @internal */
export const enum PackageJsonDependencyGroup {
    Dependencies = 1 << 0,
    DevDependencies = 1 << 1,
    PeerDependencies = 1 << 2,
    OptionalDependencies = 1 << 3,
    All = Dependencies | DevDependencies | PeerDependencies | OptionalDependencies,
}

/**
 * Interface for information extracted from a project's package.json file.
 * @internal
 */
export interface ProjectPackageJsonInfo {
    fileName: string;
    parseable: boolean;
    dependencies?: Map<string, string>;
    devDependencies?: Map<string, string>;
    peerDependencies?: Map<string, string>;
    optionalDependencies?: Map<string, string>;
    get(dependencyName: string, inGroups?: PackageJsonDependencyGroup): string | undefined;
    has(dependencyName: string, inGroups?: PackageJsonDependencyGroup): boolean;
}

/** @internal */
export interface FormattingHost {
    getNewLine?(): string;
}

/** @internal */
export const enum PackageJsonAutoImportPreference {
    Off,
    On,
    Auto,
}

export interface PerformanceEvent {
    kind: "UpdateGraph" | "CreatePackageJsonAutoImportProvider";
    durationMs: number;
}

export enum LanguageServiceMode {
    Semantic,
    PartialSemantic,
    Syntactic,
}

export interface IncompleteCompletionsCache {
    get(): CompletionInfo | undefined;
    set(response: CompletionInfo): void;
    clear(): void;
}

//
// Public interface of the host of a language service instance.
//
/**
 * Represents a host for the LanguageService.
 * @remarks
 * This interface extends GetEffectiveTypeRootsHost and MinimalResolutionCacheHost.
 * @remarks
 * Includes methods for getting compilation settings, script file names, script versions and snapshots, and default library file name.
 * @remarks
 * Also includes methods for reading directories and files, resolving module and type references, and getting custom transformers for emit.
 * @remarks
 * Additionally includes methods for getting directories, installing packages, and writing files.
 * @remarks
 * Finally, includes methods for getting document position mapper, source file like, package JSONs, and parsed command line.
 */
export interface LanguageServiceHost extends GetEffectiveTypeRootsHost, MinimalResolutionCacheHost {
    getCompilationSettings(): CompilerOptions;
    getNewLine?(): string;
    getProjectVersion?(): string;
    getScriptFileNames(): string[];
    getScriptKind?(fileName: string): ScriptKind;
    getScriptVersion(fileName: string): string;
    getScriptSnapshot(fileName: string): IScriptSnapshot | undefined;
    getProjectReferences?(): readonly ProjectReference[] | undefined;
    getLocalizedDiagnosticMessages?(): any;
    getCancellationToken?(): HostCancellationToken;
    getCurrentDirectory(): string;
    getDefaultLibFileName(options: CompilerOptions): string;
    log?(s: string): void;
    trace?(s: string): void;
    error?(s: string): void;
    useCaseSensitiveFileNames?(): boolean;

    /*
     * LS host can optionally implement these methods to support completions for module specifiers.
     * Without these methods, only completions for ambient modules will be provided.
     */
    readDirectory?(path: string, extensions?: readonly string[], exclude?: readonly string[], include?: readonly string[], depth?: number): string[];
    realpath?(path: string): string;
    /** @internal */ createHash?(data: string): string;

    /*
     * Unlike `realpath and `readDirectory`, `readFile` and `fileExists` are now _required_
     * to properly acquire and setup source files under module: node16+ modes.
     */
    readFile(path: string, encoding?: string): string | undefined;
    fileExists(path: string): boolean;

    /*
     * LS host can optionally implement these methods to support automatic updating when new type libraries are installed
     */
    getTypeRootsVersion?(): number;

    /*
     * LS host can optionally implement this method if it wants to be completely in charge of module name resolution.
     * if implementation is omitted then language service will use built-in module resolution logic and get answers to
     * host specific questions using 'getScriptSnapshot'.
     *
     * If this is implemented, `getResolvedModuleWithFailedLookupLocationsFromCache` should be too.
     */
    /** @deprecated supply resolveModuleNameLiterals instead for resolution that can handle newer resolution modes like nodenext */
    resolveModuleNames?(moduleNames: string[], containingFile: string, reusedNames: string[] | undefined, redirectedReference: ResolvedProjectReference | undefined, options: CompilerOptions, containingSourceFile?: SourceFile): (ResolvedModule | undefined)[];
    getResolvedModuleWithFailedLookupLocationsFromCache?(modulename: string, containingFile: string, resolutionMode?: ResolutionMode): ResolvedModuleWithFailedLookupLocations | undefined;
    /** @deprecated supply resolveTypeReferenceDirectiveReferences instead for resolution that can handle newer resolution modes like nodenext */
    resolveTypeReferenceDirectives?(typeDirectiveNames: string[] | FileReference[], containingFile: string, redirectedReference: ResolvedProjectReference | undefined, options: CompilerOptions, containingFileMode?: ResolutionMode): (ResolvedTypeReferenceDirective | undefined)[];
    resolveModuleNameLiterals?(
        moduleLiterals: readonly StringLiteralLike[],
        containingFile: string,
        redirectedReference: ResolvedProjectReference | undefined,
        options: CompilerOptions,
        containingSourceFile: SourceFile,
        reusedNames: readonly StringLiteralLike[] | undefined,
    ): readonly ResolvedModuleWithFailedLookupLocations[];
    resolveTypeReferenceDirectiveReferences?<T extends FileReference | string>(
        typeDirectiveReferences: readonly T[],
        containingFile: string,
        redirectedReference: ResolvedProjectReference | undefined,
        options: CompilerOptions,
        containingSourceFile: SourceFile | undefined,
        reusedNames: readonly T[] | undefined
    ): readonly ResolvedTypeReferenceDirectiveWithFailedLookupLocations[];
    /** @internal */
    resolveLibrary?(
        libraryName: string,
        resolveFrom: string,
        options: CompilerOptions,
        libFileName: string,
    ): ResolvedModuleWithFailedLookupLocations;
    /**
     * If provided along with custom resolveLibrary, used to determine if we should redo library resolutions
     * @internal
     */
    hasInvalidatedLibResolutions?(libFileName: string): boolean;

    /** @internal */ hasInvalidatedResolutions?: HasInvalidatedResolutions;
    /** @internal */ hasChangedAutomaticTypeDirectiveNames?: HasChangedAutomaticTypeDirectiveNames;
    /** @internal */ getGlobalTypingsCacheLocation?(): string | undefined;
    /** @internal */ getSymlinkCache?(files?: readonly SourceFile[]): SymlinkCache;
    /* Lets the Program from a AutoImportProviderProject use its host project's ModuleResolutionCache */
    /** @internal */ getModuleResolutionCache?(): ModuleResolutionCache | undefined;

    /*
     * Required for full import and type reference completions.
     * These should be unprefixed names. E.g. `getDirectories("/foo/bar")` should return `["a", "b"]`, not `["/foo/bar/a", "/foo/bar/b"]`.
     */
    getDirectories?(directoryName: string): string[];

    /**
     * Gets a set of custom transformers to use during emit.
     */
    getCustomTransformers?(): CustomTransformers | undefined;

    isKnownTypesPackageName?(name: string): boolean;
    installPackage?(options: InstallPackageOptions): Promise<ApplyCodeActionCommandResult>;
    writeFile?(fileName: string, content: string): void;

    /** @internal */ getDocumentPositionMapper?(generatedFileName: string, sourceFileName?: string): DocumentPositionMapper | undefined;
    /** @internal */ getSourceFileLike?(fileName: string): SourceFileLike | undefined;
    /** @internal */ getPackageJsonsVisibleToFile?(fileName: string, rootDir?: string): readonly ProjectPackageJsonInfo[];
    /** @internal */ getNearestAncestorDirectoryWithPackageJson?(fileName: string): string | undefined;
    /** @internal */ getPackageJsonsForAutoImport?(rootDir?: string): readonly ProjectPackageJsonInfo[];
    /** @internal */ getCachedExportInfoMap?(): ExportInfoMap;
    /** @internal */ getModuleSpecifierCache?(): ModuleSpecifierCache;
    /** @internal */ setCompilerHost?(host: CompilerHost): void;
    /** @internal */ useSourceOfProjectReferenceRedirect?(): boolean;
    /** @internal */ getPackageJsonAutoImportProvider?(): Program | undefined;
    /** @internal */ sendPerformanceEvent?(kind: PerformanceEvent["kind"], durationMs: number): void;
    getParsedCommandLine?(fileName: string): ParsedCommandLine | undefined;
    /** @internal */ onReleaseParsedCommandLine?(configFileName: string, oldResolvedRef: ResolvedProjectReference | undefined, optionOptions: CompilerOptions): void;
    /** @internal */ getIncompleteCompletionsCache?(): IncompleteCompletionsCache;
}

/** @internal */
export const emptyOptions = {};

export type WithMetadata<T> = T & { metadata?: unknown; };

export const enum SemanticClassificationFormat {
    Original = "original",
    TwentyTwenty = "2020"
}

//
// Public services of a language service instance associated
// with a language service host instance
//
export interface LanguageService {
    /** This is used as a part of restarting the language service. */
    cleanupSemanticCache(): void;

    /**
     * Gets errors indicating invalid syntax in a file.
     *
     * In English, "this cdeo have, erorrs" is syntactically invalid because it has typos,
     * grammatical errors, and misplaced punctuation. Likewise, examples of syntax
     * errors in TypeScript are missing parentheses in an `if` statement, mismatched
     * curly braces, and using a reserved keyword as a variable name.
     *
     * These diagnostics are inexpensive to compute and don't require knowledge of
     * other files. Note that a non-empty result increases the likelihood of false positives
     * from `getSemanticDiagnostics`.
     *
     * While these represent the majority of syntax-related diagnostics, there are some
     * that require the type system, which will be present in `getSemanticDiagnostics`.
     *
     * @param fileName A path to the file you want syntactic diagnostics for
     */
    getSyntacticDiagnostics(fileName: string): DiagnosticWithLocation[];

    /**
     * Gets warnings or errors indicating type system issues in a given file.
     * Requesting semantic diagnostics may start up the type system and
     * run deferred work, so the first call may take longer than subsequent calls.
     *
     * Unlike the other get*Diagnostics functions, these diagnostics can potentially not
     * include a reference to a source file. Specifically, the first time this is called,
     * it will return global diagnostics with no associated location.
     *
     * To contrast the differences between semantic and syntactic diagnostics, consider the
     * sentence: "The sun is green." is syntactically correct; those are real English words with
     * correct sentence structure. However, it is semantically invalid, because it is not true.
     *
     * @param fileName A path to the file you want semantic diagnostics for
     */
    getSemanticDiagnostics(fileName: string): Diagnostic[];

    /**
     * Gets suggestion diagnostics for a specific file. These diagnostics tend to
     * proactively suggest refactors, as opposed to diagnostics that indicate
     * potentially incorrect runtime behavior.
     *
     * @param fileName A path to the file you want semantic diagnostics for
     */
    getSuggestionDiagnostics(fileName: string): DiagnosticWithLocation[];

    // TODO: Rename this to getProgramDiagnostics to better indicate that these are any
    // diagnostics present for the program level, and not just 'options' diagnostics.

    /**
     * Gets global diagnostics related to the program configuration and compiler options.
     */
    getCompilerOptionsDiagnostics(): Diagnostic[];

    /** @deprecated Use getEncodedSyntacticClassifications instead. */
    getSyntacticClassifications(fileName: string, span: TextSpan): ClassifiedSpan[];
    getSyntacticClassifications(fileName: string, span: TextSpan, format: SemanticClassificationFormat): ClassifiedSpan[] | ClassifiedSpan2020[];

    /** @deprecated Use getEncodedSemanticClassifications instead. */
    getSemanticClassifications(fileName: string, span: TextSpan): ClassifiedSpan[];
    getSemanticClassifications(fileName: string, span: TextSpan, format: SemanticClassificationFormat): ClassifiedSpan[] | ClassifiedSpan2020[];

    /** Encoded as triples of [start, length, ClassificationType]. */
    getEncodedSyntacticClassifications(fileName: string, span: TextSpan): Classifications;

    /**
     * Gets semantic highlights information for a particular file. Has two formats, an older
     * version used by VS and a format used by VS Code.
     *
     * @param fileName The path to the file
     * @param position A text span to return results within
     * @param format Which format to use, defaults to "original"
     * @returns a number array encoded as triples of [start, length, ClassificationType, ...].
     */
    getEncodedSemanticClassifications(fileName: string, span: TextSpan, format?: SemanticClassificationFormat): Classifications;

    /**
     * Gets completion entries at a particular position in a file.
     *
     * @param fileName The path to the file
     * @param position A zero-based index of the character where you want the entries
     * @param options An object describing how the request was triggered and what kinds
     * of code actions can be returned with the completions.
     * @param formattingSettings settings needed for calling formatting functions.
     */
    getCompletionsAtPosition(fileName: string, position: number, options: GetCompletionsAtPositionOptions | undefined, formattingSettings?: FormatCodeSettings): WithMetadata<CompletionInfo> | undefined;

    /**
     * Gets the extended details for a completion entry retrieved from `getCompletionsAtPosition`.
     *
     * @param fileName The path to the file
     * @param position A zero based index of the character where you want the entries
     * @param entryName The `name` from an existing completion which came from `getCompletionsAtPosition`
     * @param formatOptions How should code samples in the completions be formatted, can be undefined for backwards compatibility
     * @param source `source` property from the completion entry
     * @param preferences User settings, can be undefined for backwards compatibility
     * @param data `data` property from the completion entry
     */
    getCompletionEntryDetails(
        fileName: string,
        position: number,
        entryName: string,
        formatOptions: FormatCodeOptions | FormatCodeSettings | undefined,
        source: string | undefined,
        preferences: UserPreferences | undefined,
        data: CompletionEntryData | undefined,
    ): CompletionEntryDetails | undefined;

    getCompletionEntrySymbol(fileName: string, position: number, name: string, source: string | undefined): Symbol | undefined;

    /**
     * Gets semantic information about the identifier at a particular position in a
     * file. Quick info is what you typically see when you hover in an editor.
     *
     * @param fileName The path to the file
     * @param position A zero-based index of the character where you want the quick info
     */
    getQuickInfoAtPosition(fileName: string, position: number): QuickInfo | undefined;

    getNameOrDottedNameSpan(fileName: string, startPos: number, endPos: number): TextSpan | undefined;

    getBreakpointStatementAtPosition(fileName: string, position: number): TextSpan | undefined;

    getSignatureHelpItems(fileName: string, position: number, options: SignatureHelpItemsOptions | undefined): SignatureHelpItems | undefined;

    getRenameInfo(fileName: string, position: number, preferences: UserPreferences): RenameInfo;
    /** @deprecated Use the signature with `UserPreferences` instead. */
    getRenameInfo(fileName: string, position: number, options?: RenameInfoOptions): RenameInfo;

    findRenameLocations(fileName: string, position: number, findInStrings: boolean, findInComments: boolean, preferences: UserPreferences): readonly RenameLocation[] | undefined;
    /** @deprecated Pass `providePrefixAndSuffixTextForRename` as part of a `UserPreferences` parameter. */
    findRenameLocations(fileName: string, position: number, findInStrings: boolean, findInComments: boolean, providePrefixAndSuffixTextForRename?: boolean): readonly RenameLocation[] | undefined;

    getSmartSelectionRange(fileName: string, position: number): SelectionRange;

    /** @internal */
    getDefinitionAtPosition(fileName: string, position: number, searchOtherFilesOnly: false, stopAtAlias: boolean): readonly DefinitionInfo[] | undefined;
    /** @internal */
    getDefinitionAtPosition(fileName: string, position: number, searchOtherFilesOnly: boolean, stopAtAlias: false): readonly DefinitionInfo[] | undefined;
    getDefinitionAtPosition(fileName: string, position: number): readonly DefinitionInfo[] | undefined;
    getDefinitionAndBoundSpan(fileName: string, position: number): DefinitionInfoAndBoundSpan | undefined;
    getTypeDefinitionAtPosition(fileName: string, position: number): readonly DefinitionInfo[] | undefined;
    getImplementationAtPosition(fileName: string, position: number): readonly ImplementationLocation[] | undefined;

    getReferencesAtPosition(fileName: string, position: number): ReferenceEntry[] | undefined;
    findReferences(fileName: string, position: number): ReferencedSymbol[] | undefined;
    getDocumentHighlights(fileName: string, position: number, filesToSearch: string[]): DocumentHighlights[] | undefined;
    getFileReferences(fileName: string): ReferenceEntry[];

    getNavigateToItems(searchValue: string, maxResultCount?: number, fileName?: string, excludeDtsFiles?: boolean): NavigateToItem[];
    getNavigationBarItems(fileName: string): NavigationBarItem[];
    getNavigationTree(fileName: string): NavigationTree;

    prepareCallHierarchy(fileName: string, position: number): CallHierarchyItem | CallHierarchyItem[] | undefined;
    provideCallHierarchyIncomingCalls(fileName: string, position: number): CallHierarchyIncomingCall[];
    provideCallHierarchyOutgoingCalls(fileName: string, position: number): CallHierarchyOutgoingCall[];

    provideInlayHints(fileName: string, span: TextSpan, preferences: UserPreferences | undefined): InlayHint[]

    getOutliningSpans(fileName: string): OutliningSpan[];
    getTodoComments(fileName: string, descriptors: TodoCommentDescriptor[]): TodoComment[];
    getBraceMatchingAtPosition(fileName: string, position: number): TextSpan[];
    getIndentationAtPosition(fileName: string, position: number, options: EditorOptions | EditorSettings): number;

    getFormattingEditsForRange(fileName: string, start: number, end: number, options: FormatCodeOptions | FormatCodeSettings): TextChange[];
    getFormattingEditsForDocument(fileName: string, options: FormatCodeOptions | FormatCodeSettings): TextChange[];
    getFormattingEditsAfterKeystroke(fileName: string, position: number, key: string, options: FormatCodeOptions | FormatCodeSettings): TextChange[];

    getDocCommentTemplateAtPosition(fileName: string, position: number, options?: DocCommentTemplateOptions, formatOptions?: FormatCodeSettings): TextInsertion | undefined;

    isValidBraceCompletionAtPosition(fileName: string, position: number, openingBrace: number): boolean;
    /**
     * This will return a defined result if the position is after the `>` of the opening tag, or somewhere in the text, of a JSXElement with no closing tag.
     * Editors should call this after `>` is typed.
     */
    getJsxClosingTagAtPosition(fileName: string, position: number): JsxClosingTagInfo | undefined;
    getLinkedEditingRangeAtPosition(fileName: string, position: number): LinkedEditingInfo | undefined;

    getSpanOfEnclosingComment(fileName: string, position: number, onlyMultiLine: boolean): TextSpan | undefined;

    toLineColumnOffset?(fileName: string, position: number): LineAndCharacter;
    /** @internal */
    getSourceMapper(): SourceMapper;
    /** @internal */
    clearSourceMapperCache(): void;

    getCodeFixesAtPosition(fileName: string, start: number, end: number, errorCodes: readonly number[], formatOptions: FormatCodeSettings, preferences: UserPreferences): readonly CodeFixAction[];
    getCombinedCodeFix(scope: CombinedCodeFixScope, fixId: {}, formatOptions: FormatCodeSettings, preferences: UserPreferences): CombinedCodeActions;

    applyCodeActionCommand(action: CodeActionCommand, formatSettings?: FormatCodeSettings): Promise<ApplyCodeActionCommandResult>;
    applyCodeActionCommand(action: CodeActionCommand[], formatSettings?: FormatCodeSettings): Promise<ApplyCodeActionCommandResult[]>;
    applyCodeActionCommand(action: CodeActionCommand | CodeActionCommand[], formatSettings?: FormatCodeSettings): Promise<ApplyCodeActionCommandResult | ApplyCodeActionCommandResult[]>;
    /** @deprecated `fileName` will be ignored */
    applyCodeActionCommand(fileName: string, action: CodeActionCommand): Promise<ApplyCodeActionCommandResult>;
    /** @deprecated `fileName` will be ignored */
    applyCodeActionCommand(fileName: string, action: CodeActionCommand[]): Promise<ApplyCodeActionCommandResult[]>;
    /** @deprecated `fileName` will be ignored */
    applyCodeActionCommand(fileName: string, action: CodeActionCommand | CodeActionCommand[]): Promise<ApplyCodeActionCommandResult | ApplyCodeActionCommandResult[]>;

    /**
     * @param includeInteractiveActions Include refactor actions that require additional arguments to be
     * passed when calling `getEditsForRefactor`. When true, clients should inspect the `isInteractive`
     * property of each returned `RefactorActionInfo` and ensure they are able to collect the appropriate
     * arguments for any interactive action before offering it.
     */
    getApplicableRefactors(fileName: string, positionOrRange: number | TextRange, preferences: UserPreferences | undefined, triggerReason?: RefactorTriggerReason, kind?: string, includeInteractiveActions?: boolean): ApplicableRefactorInfo[];
    getEditsForRefactor(fileName: string, formatOptions: FormatCodeSettings, positionOrRange: number | TextRange, refactorName: string, actionName: string, preferences: UserPreferences | undefined, interactiveRefactorArguments?: InteractiveRefactorArguments): RefactorEditInfo | undefined;
    getMoveToRefactoringFileSuggestions(fileName: string, positionOrRange: number | TextRange, preferences: UserPreferences | undefined, triggerReason?: RefactorTriggerReason, kind?: string): { newFileName: string, files: string[] };
    organizeImports(args: OrganizeImportsArgs, formatOptions: FormatCodeSettings, preferences: UserPreferences | undefined): readonly FileTextChanges[];
    getEditsForFileRename(oldFilePath: string, newFilePath: string, formatOptions: FormatCodeSettings, preferences: UserPreferences | undefined): readonly FileTextChanges[];

    getEmitOutput(fileName: string, emitOnlyDtsFiles?: boolean, forceDtsEmit?: boolean): EmitOutput;

    getProgram(): Program | undefined;
    /** @internal */ getCurrentProgram(): Program | undefined;

    /** @internal */ getNonBoundSourceFile(fileName: string): SourceFile;
    /** @internal */ getAutoImportProvider(): Program | undefined;

    /// Returns true if a suitable symbol was found in the project.
    /// May set isDefinition properties in `referencedSymbols` to false.
    /// May add elements to `knownSymbolSpans`.
    /** @internal */ updateIsDefinitionOfReferencedSymbols(referencedSymbols: readonly ReferencedSymbol[], knownSymbolSpans: Set<DocumentSpan>): boolean;

    toggleLineComment(fileName: string, textRange: TextRange): TextChange[];
    toggleMultilineComment(fileName: string, textRange: TextRange): TextChange[];
    commentSelection(fileName: string, textRange: TextRange): TextChange[];
    uncommentSelection(fileName: string, textRange: TextRange): TextChange[];

    getSupportedCodeFixes(fileName?: string): readonly string[];

    dispose(): void;
}

export interface JsxClosingTagInfo {
    readonly newText: string;
}

export interface LinkedEditingInfo {
    readonly ranges: TextSpan[];
    wordPattern?: string;
}

export interface CombinedCodeFixScope { type: "file"; fileName: string; }

export const enum OrganizeImportsMode {
    All = "All",
    SortAndCombine = "SortAndCombine",
    RemoveUnused = "RemoveUnused",
}

export interface OrganizeImportsArgs extends CombinedCodeFixScope {
    /** @deprecated Use `mode` instead */
    skipDestructiveCodeActions?: boolean;
    mode?: OrganizeImportsMode;
}

export type CompletionsTriggerCharacter = "." | '"' | "'" | "`" | "/" | "@" | "<" | "#" | " ";

/**
 * Represents the kind of completion trigger.
 * @readonly
 * @enum {number}
 * @property {number} Invoked - Completion was triggered by typing an identifier, manual invocation (e.g Ctrl+Space) or via API.
 * @property {number} TriggerCharacter - Completion was triggered by a trigger character.
 * @property {number} TriggerForIncompleteCompletions - Completion was re-triggered as the current completion list is incomplete.
 */
export const enum CompletionTriggerKind {
    /** Completion was triggered by typing an identifier, manual invocation (e.g Ctrl+Space) or via API. */
    Invoked = 1,

    /** Completion was triggered by a trigger character. */
    TriggerCharacter = 2,

    /** Completion was re-triggered as the current completion list is incomplete. */
    TriggerForIncompleteCompletions = 3,
}

/**
 * Options for getting completions at a specific position in the code.
 * @extends UserPreferences
 * @remarks
 * Use `triggerCharacter` and `triggerKind` to specify when the editor is requesting completions.
 * Use `includeSymbol` to include a `symbol` property on each completion entry object, but be cautious when serializing or retaining completion entries retrieved with this option.
 * @deprecated Use `includeCompletionsForModuleExports` instead of `includeExternalModuleExports`.
 * @deprecated Use `includeCompletionsWithInsertText` instead of `includeInsertTextCompletions`.
 */
export interface GetCompletionsAtPositionOptions extends UserPreferences {
    /**
     * If the editor is asking for completions because a certain character was typed
     * (as opposed to when the user explicitly requested them) this should be set.
     */
    triggerCharacter?: CompletionsTriggerCharacter;
    triggerKind?: CompletionTriggerKind;
    /**
     * Include a `symbol` property on each completion entry object.
     * Symbols reference cyclic data structures and sometimes an entire TypeChecker instance,
     * so use caution when serializing or retaining completion entries retrieved with this option.
     * @default false
     */
    includeSymbol?: boolean
    /** @deprecated Use includeCompletionsForModuleExports */
    includeExternalModuleExports?: boolean;
    /** @deprecated Use includeCompletionsWithInsertText */
    includeInsertTextCompletions?: boolean;
}

export type SignatureHelpTriggerCharacter = "," | "(" | "<";
export type SignatureHelpRetriggerCharacter = SignatureHelpTriggerCharacter | ")";

export interface SignatureHelpItemsOptions {
    triggerReason?: SignatureHelpTriggerReason;
}

export type SignatureHelpTriggerReason =
    | SignatureHelpInvokedReason
    | SignatureHelpCharacterTypedReason
    | SignatureHelpRetriggeredReason;

/**
 * Signals that the user manually requested signature help.
 * The language service will unconditionally attempt to provide a result.
 */
export interface SignatureHelpInvokedReason {
    kind: "invoked";
    triggerCharacter?: undefined;
}

/**
 * Signals that the signature help request came from a user typing a character.
 * Depending on the character and the syntactic context, the request may or may not be served a result.
 */
export interface SignatureHelpCharacterTypedReason {
    kind: "characterTyped";
    /**
     * Character that was responsible for triggering signature help.
     */
    triggerCharacter: SignatureHelpTriggerCharacter;
}

/**
 * Signals that this signature help request came from typing a character or moving the cursor.
 * This should only occur if a signature help session was already active and the editor needs to see if it should adjust.
 * The language service will unconditionally attempt to provide a result.
 * `triggerCharacter` can be `undefined` for a retrigger caused by a cursor move.
 */
export interface SignatureHelpRetriggeredReason {
    kind: "retrigger";
    /**
     * Character that was responsible for triggering signature help.
     */
    triggerCharacter?: SignatureHelpRetriggerCharacter;
}

export interface ApplyCodeActionCommandResult {
    successMessage: string;
}

export interface Classifications {
    spans: number[];
    endOfLineState: EndOfLineState;
}

export interface ClassifiedSpan {
    textSpan: TextSpan;
    classificationType: ClassificationTypeNames;
}

export interface ClassifiedSpan2020 {
    textSpan: TextSpan;
    classificationType: number;
}

/**
 * Interface for a navigation bar item.
 * @param {string} text - The text displayed for the item.
 * @param {ScriptElementKind} kind - The kind of script element represented by the item.
 * @param {string} kindModifiers - The modifiers for the script element.
 * @param {TextSpan[]} spans - The text spans of the script element.
 * @param {NavigationBarItem[]} childItems - The child items of the navigation bar item.
 * @param {number} indent - The indentation level of the item.
 * @param {boolean} bolded - Whether the item should be displayed in bold.
 * @param {boolean} grayed - Whether the item should be displayed in gray.
 */
export interface NavigationBarItem {
    text: string;
    kind: ScriptElementKind;
    kindModifiers: string;
    spans: TextSpan[];
    childItems: NavigationBarItem[];
    indent: number;
    bolded: boolean;
    grayed: boolean;
}

/**
 * Represents a node in a tree of nested declarations in a file.
 * The top node is always a script or module node.
 * @interface
 * @property {string} text - Name of the declaration, or a short description, e.g. "<class>".
 * @property {ScriptElementKind} kind - The kind of the script element.
 * @property {string} kindModifiers - ScriptElementKindModifier separated by commas, e.g. "public,abstract".
 * @property {TextSpan[]} spans - Spans of the nodes that generated this declaration. There will be more than one if this is the result of merging.
 * @property {TextSpan|undefined} nameSpan - The span of the name of the declaration, if present.
 * @property {NavigationTree[]|undefined} childItems - An array of child NavigationTree nodes, if non-empty.
 */
export interface NavigationTree {
    /** Name of the declaration, or a short description, e.g. "<class>". */
    text: string;
    kind: ScriptElementKind;
    /** ScriptElementKindModifier separated by commas, e.g. "public,abstract" */
    kindModifiers: string;
    /**
     * Spans of the nodes that generated this declaration.
     * There will be more than one if this is the result of merging.
     */
    spans: TextSpan[];
    nameSpan: TextSpan | undefined;
    /** Present if non-empty */
    childItems?: NavigationTree[];
}

export interface CallHierarchyItem {
    name: string;
    kind: ScriptElementKind;
    kindModifiers?: string;
    file: string;
    span: TextSpan;
    selectionSpan: TextSpan;
    containerName?: string;
}

export interface CallHierarchyIncomingCall {
    from: CallHierarchyItem;
    fromSpans: TextSpan[];
}

export interface CallHierarchyOutgoingCall {
    to: CallHierarchyItem;
    fromSpans: TextSpan[];
}

export const enum InlayHintKind {
    Type = "Type",
    Parameter = "Parameter",
    Enum = "Enum",
}

export interface InlayHint {
    text: string;
    position: number;
    kind: InlayHintKind;
    whitespaceBefore?: boolean;
    whitespaceAfter?: boolean;
}

export interface TodoCommentDescriptor {
    text: string;
    priority: number;
}

export interface TodoComment {
    descriptor: TodoCommentDescriptor;
    message: string;
    position: number;
}

export interface TextChange {
    span: TextSpan;
    newText: string;
}

export interface FileTextChanges {
    fileName: string;
    textChanges: readonly TextChange[];
    isNewFile?: boolean;
}

/**
 * Represents a code action that can be displayed in the UI of the editor.
 * @interface
 * @property {string} description - Description of the code action to display in the UI of the editor.
 * @property {FileTextChanges[]} changes - Text changes to apply to each file as part of the code action.
 * @property {CodeActionCommand[]} [commands] - If the user accepts the code fix, the editor should send the action back in a `applyAction` request. This allows the language service to have side effects (e.g. installing dependencies) upon a code fix.
 */
export interface CodeAction {
    /** Description of the code action to display in the UI of the editor */
    description: string;
    /** Text changes to apply to each file as part of the code action */
    changes: FileTextChanges[];
    /**
     * If the user accepts the code fix, the editor should send the action back in a `applyAction` request.
     * This allows the language service to have side effects (e.g. installing dependencies) upon a code fix.
     */
    commands?: CodeActionCommand[];
}

/**
 * Represents a code fix action that extends the CodeAction interface.
 * @interface
 * @extends CodeAction
 * @property {string} fixName - Short name to identify the fix, for use by telemetry.
 * @property {Object} [fixId] - If present, one may call 'getCombinedCodeFix' with this fixId.
 * This may be omitted to indicate that the code fix can't be applied in a group.
 * @property {string} [fixAllDescription] - Description of the fix when applied to all occurrences.
 */
export interface CodeFixAction extends CodeAction {
    /** Short name to identify the fix, for use by telemetry. */
    fixName: string;
    /**
     * If present, one may call 'getCombinedCodeFix' with this fixId.
     * This may be omitted to indicate that the code fix can't be applied in a group.
     */
    fixId?: {};
    fixAllDescription?: string;
}

export interface CombinedCodeActions {
    changes: readonly FileTextChanges[];
    commands?: readonly CodeActionCommand[];
}

// Publicly, this type is just `{}`. Internally it is a union of all the actions we use.
// See `commands?: {}[]` in protocol.ts
export type CodeActionCommand = InstallPackageAction;

export interface InstallPackageAction {
    /** @internal */ readonly type: "install package";
    /** @internal */ readonly file: string;
    /** @internal */ readonly packageName: string;
}

/**
 * Represents a set of one or more available refactoring actions, grouped under a parent refactoring.
 * @interface
 */
export interface ApplicableRefactorInfo {
    /**
     * The programmatic name of the refactoring
     */
    name: string;
    /**
     * A description of this refactoring category to show to the user.
     * If the refactoring gets inlined (see below), this text will not be visible.
     */
    description: string;
    /**
     * Inlineable refactorings can have their actions hoisted out to the top level
     * of a context menu. Non-inlineanable refactorings should always be shown inside
     * their parent grouping.
     *
     * If not specified, this value is assumed to be 'true'
     */
    inlineable?: boolean;

    actions: RefactorActionInfo[];
}

/**
 * Represents information about a single refactoring action.
 * @interface
 * @property {string} name - The programmatic name of the refactoring action.
 * @property {string} description - A description of this refactoring action to show to the user.
 * @property {string} [notApplicableReason] - A message to show to the user if the refactoring cannot be applied in the current context.
 * @property {string} [kind] - The hierarchical dotted name of the refactor action.
 * @property {boolean} [isInteractive] - Indicates that the action requires additional arguments to be passed when calling `getEditsForRefactor`.
 */
export interface RefactorActionInfo {
    /**
     * The programmatic name of the refactoring action
     */
    name: string;

    /**
     * A description of this refactoring action to show to the user.
     * If the parent refactoring is inlined away, this will be the only text shown,
     * so this description should make sense by itself if the parent is inlineable=true
     */
    description: string;

    /**
     * A message to show to the user if the refactoring cannot be applied in
     * the current context.
     */
    notApplicableReason?: string;

    /**
     * The hierarchical dotted name of the refactor action.
     */
    kind?: string;

    /**
     * Indicates that the action requires additional arguments to be passed
     * when calling `getEditsForRefactor`.
     */
    isInteractive?: boolean;
}

/**
 * A set of edits to make in response to a refactor action, plus an optional
 * location where renaming should be invoked from
 */
export interface RefactorEditInfo {
    edits: FileTextChanges[];
    renameFilename?: string;
    renameLocation?: number;
    commands?: CodeActionCommand[];
    notApplicableReason?: string;
}

export type RefactorTriggerReason = "implicit" | "invoked";

export interface TextInsertion {
    newText: string;
    /** The position in newText the caret should point to after the insertion. */
    caretOffset: number;
}

/**
 * Represents a span of text within a document, along with relevant context information.
 * @interface
 * @property {TextSpan} textSpan - The span of text within the document.
 * @property {string} fileName - The name of the file containing the text span.
 * @property {TextSpan} [originalTextSpan] - If the span represents a location that was remapped, the original text span is specified here.
 * @property {string} [originalFileName] - If the span represents a location that was remapped, the original file name is specified here.
 * @property {TextSpan} [contextSpan] - If the text span is for the name of a declaration, this is the span for the relevant declaration.
 * @property {TextSpan} [originalContextSpan] - If the text span is for the name of a declaration that was remapped, the original context span is specified here.
 */
export interface DocumentSpan {
    textSpan: TextSpan;
    fileName: string;

    /**
     * If the span represents a location that was remapped (e.g. via a .d.ts.map file),
     * then the original filename and span will be specified here
     */
    originalTextSpan?: TextSpan;
    originalFileName?: string;

    /**
     * If DocumentSpan.textSpan is the span for name of the declaration,
     * then this is the span for relevant declaration
     */
    contextSpan?: TextSpan;
    originalContextSpan?: TextSpan;
}

export interface RenameLocation extends DocumentSpan {
    readonly prefixText?: string;
    readonly suffixText?: string;
}

export interface ReferenceEntry extends DocumentSpan {
    isWriteAccess: boolean;
    isInString?: true;
}

export interface ImplementationLocation extends DocumentSpan {
    kind: ScriptElementKind;
    displayParts: SymbolDisplayPart[];
}

export const enum HighlightSpanKind {
    none = "none",
    definition = "definition",
    reference = "reference",
    writtenReference = "writtenReference",
}

export interface HighlightSpan {
    fileName?: string;
    isInString?: true;
    textSpan: TextSpan;
    contextSpan?: TextSpan;
    kind: HighlightSpanKind;
}

/**
 * Interface for navigating to a specific item.
 * @param {string} name - The name of the item to navigate to.
 * @param {ScriptElementKind} kind - The kind of script element.
 * @param {string} kindModifiers - The modifiers of the script element.
 * @param {"exact" | "prefix" | "substring" | "camelCase"} matchKind - The type of matching to use.
 * @param {boolean} isCaseSensitive - Whether the matching is case sensitive.
 * @param {string} fileName - The name of the file containing the item.
 * @param {TextSpan} textSpan - The text span of the item.
 * @param {string} containerName - The name of the container of the item.
 * @param {ScriptElementKind} containerKind - The kind of the container of the item.
 */
export interface NavigateToItem {
    name: string;
    kind: ScriptElementKind;
    kindModifiers: string;
    matchKind: "exact" | "prefix" | "substring" | "camelCase";
    isCaseSensitive: boolean;
    fileName: string;
    textSpan: TextSpan;
    containerName: string;
    containerKind: ScriptElementKind;
}

export enum IndentStyle {
    None = 0,
    Block = 1,
    Smart = 2,
}

export enum SemicolonPreference {
    Ignore = "ignore",
    Insert = "insert",
    Remove = "remove",
}

/** @deprecated - consider using EditorSettings instead */
export interface EditorOptions {
    BaseIndentSize?: number;
    IndentSize: number;
    TabSize: number;
    NewLineCharacter: string;
    ConvertTabsToSpaces: boolean;
    IndentStyle: IndentStyle;
}

// TODO: GH#18217 These are frequently asserted as defined
export interface EditorSettings {
    baseIndentSize?: number;
    indentSize?: number;
    tabSize?: number;
    newLineCharacter?: string;
    convertTabsToSpaces?: boolean;
    indentStyle?: IndentStyle;
    trimTrailingWhitespace?: boolean;
}

/**
 * Interface for specifying options for formatting code.
 * @extends EditorOptions
 * @remarks
 * This interface is used to specify options for formatting code. It extends the EditorOptions interface.
 * @deprecated - consider using FormatCodeSettings instead
 * @public
 */
export interface FormatCodeOptions extends EditorOptions {
    InsertSpaceAfterCommaDelimiter: boolean;
    InsertSpaceAfterSemicolonInForStatements: boolean;
    InsertSpaceBeforeAndAfterBinaryOperators: boolean;
    InsertSpaceAfterConstructor?: boolean;
    InsertSpaceAfterKeywordsInControlFlowStatements: boolean;
    InsertSpaceAfterFunctionKeywordForAnonymousFunctions: boolean;
    InsertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis: boolean;
    InsertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets: boolean;
    InsertSpaceAfterOpeningAndBeforeClosingNonemptyBraces?: boolean;
    InsertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces: boolean;
    InsertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces?: boolean;
    InsertSpaceAfterTypeAssertion?: boolean;
    InsertSpaceBeforeFunctionParenthesis?: boolean;
    PlaceOpenBraceOnNewLineForFunctions: boolean;
    PlaceOpenBraceOnNewLineForControlBlocks: boolean;
    insertSpaceBeforeTypeAnnotation?: boolean;
}

/**
 * Interface for settings related to formatting code.
 * Extends EditorSettings interface.
 * @readonly
 * @interface
 * @property {boolean} [insertSpaceAfterCommaDelimiter] - Whether to insert a space after a comma delimiter.
 * @property {boolean} [insertSpaceAfterSemicolonInForStatements] - Whether to insert a space after a semicolon in for statements.
 * @property {boolean} [insertSpaceBeforeAndAfterBinaryOperators] - Whether to insert a space before and after binary operators.
 * @property {boolean} [insertSpaceAfterConstructor] - Whether to insert a space after a constructor.
 * @property {boolean} [insertSpaceAfterKeywordsInControlFlowStatements] - Whether to insert a space after keywords in control flow statements.
 * @property {boolean} [insertSpaceAfterFunctionKeywordForAnonymousFunctions] - Whether to insert a space after the function keyword for anonymous functions.
 * @property {boolean} [insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis] - Whether to insert a space after opening and before closing non-empty parenthesis.
 * @property {boolean} [insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets] - Whether to insert a space after opening and before closing non-empty brackets.
 * @property {boolean} [insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces] - Whether to insert a space after opening and before closing non-empty braces.
 * @property {boolean} [insertSpaceAfterOpeningAndBeforeClosingEmptyBraces] - Whether to insert a space after opening and before closing empty braces.
 * @property {boolean} [insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces] - Whether to insert a space after opening and before closing template string braces.
 * @property {boolean} [insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces] - Whether to insert a space after opening and before closing JSX expression braces.
 * @property {boolean} [insertSpaceAfterTypeAssertion] - Whether to insert a space after a type assertion.
 * @property {boolean} [insertSpaceBeforeFunctionParenthesis] - Whether to insert a space before function parenthesis.
 * @property {boolean} [placeOpenBraceOnNewLineForFunctions] - Whether to place an open brace on a new line for functions.
 * @property {boolean} [placeOpenBraceOnNewLineForControlBlocks] - Whether to place an open brace on a new line for control blocks.
 * @property {boolean} [insertSpaceBeforeTypeAnnotation] - Whether to insert a space before a type annotation.
 * @property {boolean} [indentMultiLineObjectLiteralBeginningOnBlankLine] - Whether to indent multi-line object literal beginning on a blank line.
 * @property {SemicolonPreference} [semicolons] - Semicolon preference.
 * @property {boolean} [indentSwitchCase] - Whether to indent switch case.
 */
export interface FormatCodeSettings extends EditorSettings {
    readonly insertSpaceAfterCommaDelimiter?: boolean;
    readonly insertSpaceAfterSemicolonInForStatements?: boolean;
    readonly insertSpaceBeforeAndAfterBinaryOperators?: boolean;
    readonly insertSpaceAfterConstructor?: boolean;
    readonly insertSpaceAfterKeywordsInControlFlowStatements?: boolean;
    readonly insertSpaceAfterFunctionKeywordForAnonymousFunctions?: boolean;
    readonly insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis?: boolean;
    readonly insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets?: boolean;
    readonly insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces?: boolean;
    readonly insertSpaceAfterOpeningAndBeforeClosingEmptyBraces?: boolean;
    readonly insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces?: boolean;
    readonly insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces?: boolean;
    readonly insertSpaceAfterTypeAssertion?: boolean;
    readonly insertSpaceBeforeFunctionParenthesis?: boolean;
    readonly placeOpenBraceOnNewLineForFunctions?: boolean;
    readonly placeOpenBraceOnNewLineForControlBlocks?: boolean;
    readonly insertSpaceBeforeTypeAnnotation?: boolean;
    readonly indentMultiLineObjectLiteralBeginningOnBlankLine?: boolean;
    readonly semicolons?: SemicolonPreference;
    readonly indentSwitchCase?: boolean;
}

/**
 * Returns default format code settings.
 * @param {string} [newLineCharacter] - The new line character to use. Defaults to "\n".
 * @returns {FormatCodeSettings} - The default format code settings object.
 */
export function getDefaultFormatCodeSettings(newLineCharacter?: string): FormatCodeSettings {
    return {
        indentSize: 4,
        tabSize: 4,
        newLineCharacter: newLineCharacter || "\n",
        convertTabsToSpaces: true,
        indentStyle: IndentStyle.Smart,
        insertSpaceAfterConstructor: false,
        insertSpaceAfterCommaDelimiter: true,
        insertSpaceAfterSemicolonInForStatements: true,
        insertSpaceBeforeAndAfterBinaryOperators: true,
        insertSpaceAfterKeywordsInControlFlowStatements: true,
        insertSpaceAfterFunctionKeywordForAnonymousFunctions: false,
        insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis: false,
        insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets: false,
        insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces: true,
        insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces: false,
        insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces: false,
        insertSpaceBeforeFunctionParenthesis: false,
        placeOpenBraceOnNewLineForFunctions: false,
        placeOpenBraceOnNewLineForControlBlocks: false,
        semicolons: SemicolonPreference.Ignore,
        trimTrailingWhitespace: true,
        indentSwitchCase: true
    };
}

/** @internal */
export const testFormatSettings = getDefaultFormatCodeSettings("\n");

/**
 * Represents information about a definition in a TypeScript document.
 * @interface
 * @extends DocumentSpan
 * @property {ScriptElementKind} kind - The kind of script element.
 * @property {string} name - The name of the script element.
 * @property {ScriptElementKind} containerKind - The kind of container for the script element.
 * @property {string} containerName - The name of the container for the script element.
 * @property {boolean} [unverified] - Indicates if the definition is unverified.
 * @property {boolean} [isLocal] - Indicates if the definition is local.
 * @property {boolean} [isAmbient] - Indicates if the definition is ambient.
 * @property {boolean} [failedAliasResolution] - Indicates if the alias resolution failed.
 */
export interface DefinitionInfo extends DocumentSpan {
    kind: ScriptElementKind;
    name: string;
    containerKind: ScriptElementKind;
    containerName: string;
    unverified?: boolean;
    /** @internal
     * Initially, this value is determined syntactically, but it is updated by the checker to cover
     * cases like declarations that are exported in subsequent statements.  As a result, the value
     * may be "incomplete" if this span has yet to be checked.
     */
    isLocal?: boolean;
    /** @internal */ isAmbient?: boolean;
    /** @internal */ failedAliasResolution?: boolean;
}

export interface DefinitionInfoAndBoundSpan {
    definitions?: readonly DefinitionInfo[];
    textSpan: TextSpan;
}

export interface ReferencedSymbolDefinitionInfo extends DefinitionInfo {
    displayParts: SymbolDisplayPart[];
}

export interface ReferencedSymbol {
    definition: ReferencedSymbolDefinitionInfo;
    references: ReferencedSymbolEntry[];
}

export interface ReferencedSymbolEntry extends ReferenceEntry {
    isDefinition?: boolean;
}

/**
 * An enumeration of possible kinds of symbol display parts.
 * @enum {number}
 * @readonly
 * @property {number} aliasName - The display part represents an alias name.
 * @property {number} className - The display part represents a class name.
 * @property {number} enumName - The display part represents an enum name.
 * @property {number} fieldName - The display part represents a field name.
 * @property {number} interfaceName - The display part represents an interface name.
 * @property {number} keyword - The display part represents a keyword.
 * @property {number} lineBreak - The display part represents a line break.
 * @property {number} numericLiteral - The display part represents a numeric literal.
 * @property {number} stringLiteral - The display part represents a string literal.
 * @property {number} localName - The display part represents a local name.
 * @property {number} methodName - The display part represents a method name.
 * @property {number} moduleName - The display part represents a module name.
 * @property {number} operator - The display part represents an operator.
 * @property {number} parameterName - The display part represents a parameter name.
 * @property {number} propertyName - The display part represents a property name.
 * @property {number} punctuation - The display part represents a punctuation mark.
 * @property {number} space - The display part represents a space.
 * @property {number} text - The display part represents plain text.
 * @property {number} typeParameterName - The display part represents a type parameter name.
 * @property {number} enumMemberName - The display part represents an enum member name.
 * @property {number} functionName - The display part represents a function name.
 * @property {number} regularExpressionLiteral - The display part represents a regular expression literal.
 * @property {number} link - The display part represents a link.
 * @property {number} linkName - The display part represents a link name.
 * @property {number} linkText - The display part represents link text.
 */
export enum SymbolDisplayPartKind {
    aliasName,
    className,
    enumName,
    fieldName,
    interfaceName,
    keyword,
    lineBreak,
    numericLiteral,
    stringLiteral,
    localName,
    methodName,
    moduleName,
    operator,
    parameterName,
    propertyName,
    punctuation,
    space,
    text,
    typeParameterName,
    enumMemberName,
    functionName,
    regularExpressionLiteral,
    link,
    linkName,
    linkText,
}

export interface SymbolDisplayPart {
    text: string;
    kind: string;
}

export interface JSDocLinkDisplayPart extends SymbolDisplayPart {
    target: DocumentSpan;
}

export interface JSDocTagInfo {
    name: string;
    text?: SymbolDisplayPart[];
}

export interface QuickInfo {
    kind: ScriptElementKind;
    kindModifiers: string;
    textSpan: TextSpan;
    displayParts?: SymbolDisplayPart[];
    documentation?: SymbolDisplayPart[];
    tags?: JSDocTagInfo[];
}

export type RenameInfo = RenameInfoSuccess | RenameInfoFailure;
/**
 * Interface for successful rename information.
 * @interface
 * @property {boolean} canRename - Indicates if the rename operation can be performed.
 * @property {string | undefined} fileToRename - File or directory to rename. If set, `getEditsForFileRename` should be called instead of `findRenameLocations`.
 * @property {string} displayName - Display name of the renamed item.
 * @property {string} fullDisplayName - Full display name of the renamed item.
 * @property {ScriptElementKind} kind - Kind of the renamed item.
 * @property {string} kindModifiers - Kind modifiers of the renamed item.
 * @property {TextSpan} triggerSpan - Text span of the renamed item.
 */
export interface RenameInfoSuccess {
    canRename: true;
    /**
     * File or directory to rename.
     * If set, `getEditsForFileRename` should be called instead of `findRenameLocations`.
     */
    fileToRename?: string;
    displayName: string;
    fullDisplayName: string;
    kind: ScriptElementKind;
    kindModifiers: string;
    triggerSpan: TextSpan;
}
export interface RenameInfoFailure {
    canRename: false;
    localizedErrorMessage: string;
}

/**
 * @deprecated Use `UserPreferences` instead.
 */
export interface RenameInfoOptions {
    readonly allowRenameOfImportPath?: boolean;
}

export interface DocCommentTemplateOptions {
    readonly generateReturnInDocTemplate?: boolean;
}

export interface InteractiveRefactorArguments {
    targetFile: string;
}

export interface SignatureHelpParameter {
    name: string;
    documentation: SymbolDisplayPart[];
    displayParts: SymbolDisplayPart[];
    isOptional: boolean;
    isRest?: boolean;
}

export interface SelectionRange {
    textSpan: TextSpan;
    parent?: SelectionRange;
}

/**
 * Represents a single signature to show in signature help.
 * The id is used for subsequent calls into the language service to ask questions about the
 * signature help item in the context of any documents that have been updated.  i.e. after
 * an edit has happened, while signature help is still active, the host can ask important
 * questions like 'what parameter is the user currently contained within?'.
 */
export interface SignatureHelpItem {
    isVariadic: boolean;
    prefixDisplayParts: SymbolDisplayPart[];
    suffixDisplayParts: SymbolDisplayPart[];
    separatorDisplayParts: SymbolDisplayPart[];
    parameters: SignatureHelpParameter[];
    documentation: SymbolDisplayPart[];
    tags: JSDocTagInfo[];
}

/**
 * Represents a set of signature help items, and the preferred item that should be selected.
 */
export interface SignatureHelpItems {
    items: SignatureHelpItem[];
    applicableSpan: TextSpan;
    selectedItemIndex: number;
    argumentIndex: number;
    argumentCount: number;
}

// Do not change existing values, as they exist in telemetry.
export const enum CompletionInfoFlags {
    None = 0,
    MayIncludeAutoImports = 1 << 0,
    IsImportStatementCompletion = 1 << 1,
    IsContinuation = 1 << 2,
    ResolvedModuleSpecifiers = 1 << 3,
    ResolvedModuleSpecifiersBeyondLimit = 1 << 4,
    MayIncludeMethodSnippets = 1 << 5,
}

/**
 * Interface for completion information.
 * @param {CompletionInfoFlags} [flags] - Flags for performance telemetry.
 * @param {boolean} isGlobalCompletion - True if the enclosing scope matches a few syntax kinds.
 * @param {boolean} isMemberCompletion - True if the completion is a member of an object.
 * @param {TextSpan} [optionalReplacementSpan] - Optional replacement span for the completion entry.
 * @param {boolean} isNewIdentifierLocation - True when the current location allows for a new identifier.
 * @param {boolean} [isIncomplete] - Indicates to client to continue requesting completions on subsequent keystrokes.
 * @param {CompletionEntry[]} entries - Array of completion entries.
 */
export interface CompletionInfo {
    /** For performance telemetry. */
    flags?: CompletionInfoFlags;
    /** Not true for all global completions. This will be true if the enclosing scope matches a few syntax kinds. See `isSnippetScope`. */
    isGlobalCompletion: boolean;
    isMemberCompletion: boolean;
    /**
     * In the absence of `CompletionEntry["replacementSpan"]`, the editor may choose whether to use
     * this span or its default one. If `CompletionEntry["replacementSpan"]` is defined, that span
     * must be used to commit that completion entry.
     */
    optionalReplacementSpan?: TextSpan;
    /**
     * true when the current location also allows for a new identifier
     */
    isNewIdentifierLocation: boolean;
    /**
     * Indicates to client to continue requesting completions on subsequent keystrokes.
     */
    isIncomplete?: true;
    entries: CompletionEntry[];
}

/**
 * Represents data for an auto-import completion entry.
 * @interface
 * @property {string} exportName - The name of the property or export in the module's symbol table. Differs from the completion name in the case of InternalSymbolName.ExportEquals and InternalSymbolName.Default.
 * @property {string} [exportMapKey] - The key used to map the export in the completion entry.
 * @property {string} [moduleSpecifier] - The module specifier for the import statement.
 * @property {string} [fileName] - The file name declaring the export's module symbol, if it was an external module.
 * @property {string} [ambientModuleName] - The module name (with quotes stripped) of the export's module symbol, if it was an ambient module.
 * @property {boolean} [isPackageJsonImport] - True if the export was found in the package.json AutoImportProvider.
 */
export interface CompletionEntryDataAutoImport {
    /**
     * The name of the property or export in the module's symbol table. Differs from the completion name
     * in the case of InternalSymbolName.ExportEquals and InternalSymbolName.Default.
     */
    exportName: string;
    exportMapKey?: string;
    moduleSpecifier?: string;
    /** The file name declaring the export's module symbol, if it was an external module */
    fileName?: string;
    /** The module name (with quotes stripped) of the export's module symbol, if it was an ambient module */
    ambientModuleName?: string;
    /** True if the export was found in the package.json AutoImportProvider */
    isPackageJsonImport?: true;
}

export interface CompletionEntryDataUnresolved extends CompletionEntryDataAutoImport {
    exportMapKey: string;
}

export interface CompletionEntryDataResolved extends CompletionEntryDataAutoImport {
    moduleSpecifier: string;
}

export type CompletionEntryData = CompletionEntryDataUnresolved | CompletionEntryDataResolved;

// see comments in protocol.ts
/**
 * Represents a completion entry for an editor or IDE to display to the user.
 * @interface
 * @property {string} name - The name of the completion entry.
 * @property {ScriptElementKind} kind - The kind of the completion entry.
 * @property {string=} kindModifiers - The kind modifiers of the completion entry.
 * @property {string} sortText - The sort text of the completion entry.
 * @property {string=} insertText - The text to be inserted for the completion entry.
 * @property {string=} filterText - The text to be used for filtering the completion entry.
 * @property {boolean=} isSnippet - Indicates whether the completion entry is a snippet.
 * @property {TextSpan=} replacementSpan - The span of the text to be replaced by the completion entry.
 * @property {boolean=} hasAction - Indicates whether the completion entry has an action.
 * @property {string=} source - The source of the completion entry.
 * @property {SymbolDisplayPart[]=} sourceDisplay - The display parts of the source of the completion entry.
 * @property {CompletionEntryLabelDetails=} labelDetails - The label details of the completion entry.
 * @property {boolean=} isRecommended - Indicates whether the completion entry is recommended.
 * @property {boolean=} isFromUncheckedFile - Indicates whether the completion entry is from an unchecked file.
 * @property {boolean=} isPackageJsonImport - Indicates whether the completion entry is a package.json import.
 * @property {boolean=} isImportStatementCompletion - Indicates whether the completion entry is an import statement completion.
 * @property {Symbol=} symbol - The symbol of the completion entry.
 * @property {CompletionEntryData=} data - The data of the completion entry.
 */
export interface CompletionEntry {
    name: string;
    kind: ScriptElementKind;
    kindModifiers?: string; // see ScriptElementKindModifier, comma separated
    sortText: string;
    insertText?: string;
    filterText?: string;
    isSnippet?: true;
    /**
     * An optional span that indicates the text to be replaced by this completion item.
     * If present, this span should be used instead of the default one.
     * It will be set if the required span differs from the one generated by the default replacement behavior.
     */
    replacementSpan?: TextSpan;
    hasAction?: true;
    source?: string;
    sourceDisplay?: SymbolDisplayPart[];
    labelDetails?: CompletionEntryLabelDetails;
    isRecommended?: true;
    isFromUncheckedFile?: true;
    isPackageJsonImport?: true;
    isImportStatementCompletion?: true;
    /**
     * For API purposes.
     * Included for non-string completions only when `includeSymbol: true` option is passed to `getCompletionsAtPosition`.
     * @example Get declaration of completion: `symbol.valueDeclaration`
     */
    symbol?: Symbol
    /**
     * A property to be sent back to TS Server in the CompletionDetailsRequest, along with `name`,
     * that allows TS Server to look up the symbol represented by the completion item, disambiguating
     * items with the same name. Currently only defined for auto-import completions, but the type is
     * `unknown` in the protocol, so it can be changed as needed to support other kinds of completions.
     * The presence of this property should generally not be used to assume that this completion entry
     * is an auto-import.
     */
    data?: CompletionEntryData;
}

export interface CompletionEntryLabelDetails {
    detail?: string;
    description?: string;
}

/**
 * Represents detailed information about a completion entry.
 * @interface
 * @property {string} name - The name of the completion entry.
 * @property {ScriptElementKind} kind - The kind of the completion entry.
 * @property {string} kindModifiers - The modifiers of the completion entry.
 * @property {SymbolDisplayPart[]} displayParts - The display parts of the completion entry.
 * @property {SymbolDisplayPart[] | undefined} documentation - The documentation of the completion entry.
 * @property {JSDocTagInfo[] | undefined} tags - The JSDoc tags of the completion entry.
 * @property {CodeAction[] | undefined} codeActions - The code actions of the completion entry.
 * @property {SymbolDisplayPart[] | undefined} sourceDisplay - The source display of the completion entry.
 */
export interface CompletionEntryDetails {
    name: string;
    kind: ScriptElementKind;
    kindModifiers: string;   // see ScriptElementKindModifier, comma separated
    displayParts: SymbolDisplayPart[];
    documentation?: SymbolDisplayPart[];
    tags?: JSDocTagInfo[];
    codeActions?: CodeAction[];
    /** @deprecated Use `sourceDisplay` instead. */
    source?: SymbolDisplayPart[];
    sourceDisplay?: SymbolDisplayPart[];
}

/**
 * Represents a collapsible region within a document.
 * @interface
 * @property {TextSpan} textSpan - The span of the document to actually collapse.
 * @property {TextSpan} hintSpan - The span of the document to display when the user hovers over the collapsed span.
 * @property {string} bannerText - The text to display in the editor for the collapsed region.
 * @property {boolean} autoCollapse - Whether or not this region should be automatically collapsed when the 'Collapse to Definitions' command is invoked.
 * @property {OutliningSpanKind} kind - Classification of the contents of the span.
 */
export interface OutliningSpan {
    /** The span of the document to actually collapse. */
    textSpan: TextSpan;

    /** The span of the document to display when the user hovers over the collapsed span. */
    hintSpan: TextSpan;

    /** The text to display in the editor for the collapsed region. */
    bannerText: string;

    /**
     * Whether or not this region should be automatically collapsed when
     * the 'Collapse to Definitions' command is invoked.
     */
    autoCollapse: boolean;

    /**
     * Classification of the contents of the span
     */
    kind: OutliningSpanKind;
}

/**
 * Enum representing different kinds of outlining spans.
 * @readonly
 * @enum {string}
 * @property {string} Comment - Single or multi-line comments.
 * @property {string} Region - Sections marked by '// #region' and '// #endregion' comments.
 * @property {string} Code - Declarations and expressions.
 * @property {string} Imports - Contiguous blocks of import declarations.
 */
export const enum OutliningSpanKind {
    /** Single or multi-line comments */
    Comment = "comment",

    /** Sections marked by '// #region' and '// #endregion' comments */
    Region = "region",

    /** Declarations and expressions */
    Code = "code",

    /** Contiguous blocks of import declarations */
    Imports = "imports"
}

export const enum OutputFileType {
    JavaScript,
    SourceMap,
    Declaration
}

export const enum EndOfLineState {
    None,
    InMultiLineCommentTrivia,
    InSingleQuoteStringLiteral,
    InDoubleQuoteStringLiteral,
    InTemplateHeadOrNoSubstitutionTemplate,
    InTemplateMiddleOrTail,
    InTemplateSubstitutionPosition,
}

/**
 * Represents the different classes of tokens in a programming language.
 * @enum {number}
 * @readonly
 * @property {number} Punctuation - Represents punctuation tokens.
 * @property {number} Keyword - Represents keyword tokens.
 * @property {number} Operator - Represents operator tokens.
 * @property {number} Comment - Represents comment tokens.
 * @property {number} Whitespace - Represents whitespace tokens.
 * @property {number} Identifier - Represents identifier tokens.
 * @property {number} NumberLiteral - Represents number literal tokens.
 * @property {number} BigIntLiteral - Represents big integer literal tokens.
 * @property {number} StringLiteral - Represents string literal tokens.
 * @property {number} RegExpLiteral - Represents regular expression literal tokens.
 */
export enum TokenClass {
    Punctuation,
    Keyword,
    Operator,
    Comment,
    Whitespace,
    Identifier,
    NumberLiteral,
    BigIntLiteral,
    StringLiteral,
    RegExpLiteral,
}

export interface ClassificationResult {
    finalLexState: EndOfLineState;
    entries: ClassificationInfo[];
}

export interface ClassificationInfo {
    length: number;
    classification: TokenClass;
}

/**
 * Interface for a classifier that gives lexical classifications of tokens on a line without any syntactic context.
 */
export interface Classifier {
    /**
     * Gives lexical classifications of tokens on a line without any syntactic context.
     * For instance, a token consisting of the text 'string' can be either an identifier
     * named 'string' or the keyword 'string', however, because this classifier is not aware,
     * it relies on certain heuristics to give acceptable results. For classifications where
     * speed trumps accuracy, this function is preferable; however, for true accuracy, the
     * syntactic classifier is ideal. In fact, in certain editing scenarios, combining the
     * lexical, syntactic, and semantic classifiers may issue the best user experience.
     *
     * @param text                      The text of a line to classify.
     * @param lexState                  The state of the lexical classifier at the end of the previous line.
     * @param syntacticClassifierAbsent Whether the client is *not* using a syntactic classifier.
     *                                  If there is no syntactic classifier (syntacticClassifierAbsent=true),
     *                                  certain heuristics may be used in its place; however, if there is a
     *                                  syntactic classifier (syntacticClassifierAbsent=false), certain
     *                                  classifications which may be incorrectly categorized will be given
     *                                  back as Identifiers in order to allow the syntactic classifier to
     *                                  subsume the classification.
     * @deprecated Use getLexicalClassifications instead.
     */
    getClassificationsForLine(text: string, lexState: EndOfLineState, syntacticClassifierAbsent: boolean): ClassificationResult;
    getEncodedLexicalClassifications(text: string, endOfLineState: EndOfLineState, syntacticClassifierAbsent: boolean): Classifications;
}

/**
 * Represents the different kinds of script elements.
 * @readonly
 * @enum {string}
 * @property {string} unknown - Empty string.
 * @property {string} warning - Warning message.
 * @property {string} keyword - Predefined type (void) or keyword (class).
 * @property {string} scriptElement - Top level script node.
 * @property {string} moduleElement - Module foo {}.
 * @property {string} classElement - Class X {}.
 * @property {string} localClassElement - Var x = class X {}.
 * @property {string} interfaceElement - Interface Y {}.
 * @property {string} typeElement - Type T = ...
 * @property {string} enumElement - Enum E.
 * @property {string} enumMemberElement - Enum member.
 * @property {string} variableElement - Inside module and script only. Const v = ...
 * @property {string} localVariableElement - Inside function.
 * @property {string} functionElement - Inside module and script only. Function f() { }.
 * @property {string} localFunctionElement - Inside function.
 * @property {string} memberFunctionElement - Class X { [public|private]* foo() {} }.
 * @property {string} memberGetAccessorElement - Class X { [public|private]* [get|set] foo:number; }.
 * @property {string} memberSetAccessorElement - Class X { [public|private]* [get|set] foo:number; }.
 * @property {string} memberVariableElement - Class X { [public|private]* foo:number; }.
 * @property {string} memberAccessorVariableElement - Class X { [public|private]* accessor foo: number; }.
 * @property {string} constructorImplementationElement - Class X { constructor() { } }.
 * @property {string} callSignatureElement - Interface Y { ():number; }.
 * @property {string} indexSignatureElement - Interface Y { []:number; }.
 * @property {string} constructSignatureElement - Interface Y { new():Y; }.
 * @property {string} parameterElement - Function foo(*Y*: string).
 * @property {string} typeParameterElement - Type parameter.
 * @property {string} primitiveType - Primitive type.
 * @property {string} label - Label.
 * @property {string} alias - Alias.
 * @property {string} constElement - Const.
 * @property {string} letElement - Let.
 * @property {string} directory - Directory.
 * @property {string} externalModuleName - External module name.
 * @property {string} jsxAttribute - JSX attribute. Deprecated.
 * @property {string} string - String literal.
 * @property {string} link - Jsdoc @link.
 * @property {string} linkName - Jsdoc @link entity name.
 * @property {string} linkText - Jsdoc @link link text.
 */
export const enum ScriptElementKind {
    unknown = "",
    warning = "warning",

    /** predefined type (void) or keyword (class) */
    keyword = "keyword",

    /** top level script node */
    scriptElement = "script",

    /** module foo {} */
    moduleElement = "module",

    /** class X {} */
    classElement = "class",

    /** var x = class X {} */
    localClassElement = "local class",

    /** interface Y {} */
    interfaceElement = "interface",

    /** type T = ... */
    typeElement = "type",

    /** enum E */
    enumElement = "enum",
    enumMemberElement = "enum member",

    /**
     * Inside module and script only
     * const v = ..
     */
    variableElement = "var",

    /** Inside function */
    localVariableElement = "local var",

    /**
     * Inside module and script only
     * function f() { }
     */
    functionElement = "function",

    /** Inside function */
    localFunctionElement = "local function",

    /** class X { [public|private]* foo() {} } */
    memberFunctionElement = "method",

    /** class X { [public|private]* [get|set] foo:number; } */
    memberGetAccessorElement = "getter",
    memberSetAccessorElement = "setter",

    /**
     * class X { [public|private]* foo:number; }
     * interface Y { foo:number; }
     */
    memberVariableElement = "property",

    /** class X { [public|private]* accessor foo: number; } */
    memberAccessorVariableElement = "accessor",

    /**
     * class X { constructor() { } }
     * class X { static { } }
     */
    constructorImplementationElement = "constructor",

    /** interface Y { ():number; } */
    callSignatureElement = "call",

    /** interface Y { []:number; } */
    indexSignatureElement = "index",

    /** interface Y { new():Y; } */
    constructSignatureElement = "construct",

    /** function foo(*Y*: string) */
    parameterElement = "parameter",

    typeParameterElement = "type parameter",

    primitiveType = "primitive type",

    label = "label",

    alias = "alias",

    constElement = "const",

    letElement = "let",

    directory = "directory",

    externalModuleName = "external module name",

    /**
     * <JsxTagName attribute1 attribute2={0} />
     * @deprecated
     */
    jsxAttribute = "JSX attribute",

    /** String literal */
    string = "string",

    /** Jsdoc @link: in `{@link C link text}`, the before and after text "{@link " and "}" */
    link = "link",

    /** Jsdoc @link: in `{@link C link text}`, the entity name "C" */
    linkName = "link name",

    /** Jsdoc @link: in `{@link C link text}`, the link text "link text" */
    linkText = "link text",
}

/**
 * Enum representing various modifiers for script element kinds.
 * @readonly
 * @enum {string}
 * @property {string} none - Empty string.
 * @property {string} publicMemberModifier - Modifier for public members.
 * @property {string} privateMemberModifier - Modifier for private members.
 * @property {string} protectedMemberModifier - Modifier for protected members.
 * @property {string} exportedModifier - Modifier for exported elements.
 * @property {string} ambientModifier - Modifier for ambient declarations.
 * @property {string} staticModifier - Modifier for static members.
 * @property {string} abstractModifier - Modifier for abstract classes.
 * @property {string} optionalModifier - Modifier for optional parameters.
 * @property {string} deprecatedModifier - Modifier for deprecated elements.
 * @property {string} dtsModifier - Modifier for declaration files.
 * @property {string} tsModifier - Modifier for TypeScript files.
 * @property {string} tsxModifier - Modifier for TypeScript JSX files.
 * @property {string} jsModifier - Modifier for JavaScript files.
 * @property {string} jsxModifier - Modifier for JavaScript JSX files.
 * @property {string} jsonModifier - Modifier for JSON files.
 * @property {string} dmtsModifier - Modifier for declaration files with multiple type scripts.
 * @property {string} mtsModifier - Modifier for multiple type scripts.
 * @property {string} mjsModifier - Modifier for multiple JavaScript files.
 * @property {string} dctsModifier - Modifier for declaration files with multiple type scripts.
 * @property {string} ctsModifier - Modifier for multiple type scripts.
 * @property {string} cjsModifier - Modifier for multiple JavaScript files.
 */
export const enum ScriptElementKindModifier {
    none = "",
    publicMemberModifier = "public",
    privateMemberModifier = "private",
    protectedMemberModifier = "protected",
    exportedModifier = "export",
    ambientModifier = "declare",
    staticModifier = "static",
    abstractModifier = "abstract",
    optionalModifier = "optional",

    deprecatedModifier = "deprecated",

    dtsModifier = ".d.ts",
    tsModifier = ".ts",
    tsxModifier = ".tsx",
    jsModifier = ".js",
    jsxModifier = ".jsx",
    jsonModifier = ".json",
    dmtsModifier = ".d.mts",
    mtsModifier = ".mts",
    mjsModifier = ".mjs",
    dctsModifier = ".d.cts",
    ctsModifier = ".cts",
    cjsModifier = ".cjs",
}

/**
 * Enum representing the different types of classification names.
 * @readonly
 * @enum {string}
 * @property {string} comment - Represents a comment.
 * @property {string} identifier - Represents an identifier.
 * @property {string} keyword - Represents a keyword.
 * @property {string} numericLiteral - Represents a numeric literal.
 * @property {string} bigintLiteral - Represents a bigint literal.
 * @property {string} operator - Represents an operator.
 * @property {string} stringLiteral - Represents a string literal.
 * @property {string} whiteSpace - Represents whitespace.
 * @property {string} text - Represents text.
 * @property {string} punctuation - Represents punctuation.
 * @property {string} className - Represents a class name.
 * @property {string} enumName - Represents an enum name.
 * @property {string} interfaceName - Represents an interface name.
 * @property {string} moduleName - Represents a module name.
 * @property {string} typeParameterName - Represents a type parameter name.
 * @property {string} typeAliasName - Represents a type alias name.
 * @property {string} parameterName - Represents a parameter name.
 * @property {string} docCommentTagName - Represents a doc comment tag name.
 * @property {string} jsxOpenTagName - Represents a JSX open tag name.
 * @property {string} jsxCloseTagName - Represents a JSX close tag name.
 * @property {string} jsxSelfClosingTagName - Represents a JSX self closing tag name.
 * @property {string} jsxAttribute - Represents a JSX attribute.
 * @property {string} jsxText - Represents JSX text.
 * @property {string} jsxAttributeStringLiteralValue - Represents a JSX attribute string literal value.
 */
export const enum ClassificationTypeNames {
    comment = "comment",
    identifier = "identifier",
    keyword = "keyword",
    numericLiteral = "number",
    bigintLiteral = "bigint",
    operator = "operator",
    stringLiteral = "string",
    whiteSpace = "whitespace",
    text = "text",

    punctuation = "punctuation",

    className = "class name",
    enumName = "enum name",
    interfaceName = "interface name",
    moduleName = "module name",
    typeParameterName = "type parameter name",
    typeAliasName = "type alias name",
    parameterName = "parameter name",
    docCommentTagName = "doc comment tag name",
    jsxOpenTagName = "jsx open tag name",
    jsxCloseTagName = "jsx close tag name",
    jsxSelfClosingTagName = "jsx self closing tag name",
    jsxAttribute = "jsx attribute",
    jsxText = "jsx text",
    jsxAttributeStringLiteralValue = "jsx attribute string literal value",
}

/**
 * An enumeration of all possible types of tokens in a TypeScript source file.
 *
 * @remarks
 * This enum is used by the TypeScript compiler to categorize tokens in a source file.
 *
 * @enum {number}
 * @readonly
 * @property {number} comment - A comment token.
 * @property {number} identifier - An identifier token.
 * @property {number} keyword - A keyword token.
 * @property {number} numericLiteral - A numeric literal token.
 * @property {number} operator - An operator token.
 * @property {number} stringLiteral - A string literal token.
 * @property {number} regularExpressionLiteral - A regular expression literal token.
 * @property {number} whiteSpace - A whitespace token.
 * @property {number} text - A text token.
 * @property {number} punctuation - A punctuation token.
 * @property {number} className - A class name token.
 * @property {number} enumName - An enum name token.
 * @property {number} interfaceName - An interface name token.
 * @property {number} moduleName - A module name token.
 * @property {number} typeParameterName - A type parameter name token.
 * @property {number} typeAliasName - A type alias name token.
 * @property {number} parameterName - A parameter name token.
 * @property {number} docCommentTagName - A JSDoc tag name token.
 * @property {number} jsxOpenTagName - A JSX opening tag name token.
 * @property {number} jsxCloseTagName - A JSX closing tag name token.
 * @property {number} jsxSelfClosingTagName - A JSX self-closing tag name token.
 * @property {number} jsxAttribute - A JSX attribute token.
 * @property {number} jsxText - A JSX text token.
 * @property {number} jsxAttributeStringLiteralValue - A JSX attribute string literal value token.
 * @property {number} bigintLiteral - A bigint literal token.
 */
export const enum ClassificationType {
    comment = 1,
    identifier = 2,
    keyword = 3,
    numericLiteral = 4,
    operator = 5,
    stringLiteral = 6,
    regularExpressionLiteral = 7,
    whiteSpace = 8,
    text = 9,
    punctuation = 10,
    className = 11,
    enumName = 12,
    interfaceName = 13,
    moduleName = 14,
    typeParameterName = 15,
    typeAliasName = 16,
    parameterName = 17,
    docCommentTagName = 18,
    jsxOpenTagName = 19,
    jsxCloseTagName = 20,
    jsxSelfClosingTagName = 21,
    jsxAttribute = 22,
    jsxText = 23,
    jsxAttributeStringLiteralValue = 24,
    bigintLiteral = 25,
}

/** @internal */
export interface CodeFixRegistration {
    errorCodes: readonly number[];
    getCodeActions(context: CodeFixContext): CodeFixAction[] | undefined;
    fixIds?: readonly string[];
    getAllCodeActions?(context: CodeFixAllContext): CombinedCodeActions;
}

/** @internal */
export interface CodeFixContextBase extends textChanges.TextChangesContext {
    sourceFile: SourceFile;
    program: Program;
    cancellationToken: CancellationToken;
    preferences: UserPreferences;
}

/** @internal */
export interface CodeFixAllContext extends CodeFixContextBase {
    fixId: {};
}

/** @internal */
export interface CodeFixContext extends CodeFixContextBase {
    errorCode: number;
    span: TextSpan;
}

/** @internal */
export interface Refactor {
    /** List of action kinds a refactor can provide.
     * Used to skip unnecessary calculation when specific refactors are requested. */
    kinds?: string[];

    /** Compute the associated code actions */
    getEditsForAction(context: RefactorContext, actionName: string, interactiveRefactorArguments?: InteractiveRefactorArguments): RefactorEditInfo | undefined;

    /** Compute (quickly) which actions are available here */
    getAvailableActions(context: RefactorContext, includeInteractive?: boolean, interactiveRefactorArguments?: InteractiveRefactorArguments): readonly ApplicableRefactorInfo[];
}

/**
 * Represents the context for a refactoring operation.
 * @extends textChanges.TextChangesContext
 * @interface
 * @property {SourceFile} file - The source file being refactored.
 * @property {number} startPosition - The starting position of the refactoring operation.
 * @property {number | undefined} endPosition - The ending position of the refactoring operation, if applicable.
 * @property {Program} program - The program being used for the refactoring operation.
 * @property {CancellationToken | undefined} cancellationToken - The cancellation token for the refactoring operation, if applicable.
 * @property {UserPreferences} preferences - The user preferences for the refactoring operation.
 * @property {RefactorTriggerReason | undefined} triggerReason - The reason for triggering the refactoring operation, if applicable.
 * @property {string | undefined} kind - The kind of refactoring operation being performed, if applicable.
 */
export interface RefactorContext extends textChanges.TextChangesContext {
    file: SourceFile;
    startPosition: number;
    endPosition?: number;
    program: Program;
    cancellationToken?: CancellationToken;
    preferences: UserPreferences;
    triggerReason?: RefactorTriggerReason;
    kind?: string;
}

export interface InlayHintsContext {
    file: SourceFile;
    program: Program;
    cancellationToken: CancellationToken;
    host: LanguageServiceHost;
    span: TextSpan;
    preferences: UserPreferences;
}
