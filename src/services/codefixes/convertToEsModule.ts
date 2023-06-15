import {
    __String,
    arrayFrom,
    ArrowFunction,
    BinaryExpression,
    BindingElement,
    BindingName,
    ClassDeclaration,
    ClassExpression,
    concatenate,
    copyEntries,
    createMultiMap,
    createRange,
    Debug,
    Diagnostics,
    emptyMap,
    ExportDeclaration,
    ExportSpecifier,
    Expression,
    ExpressionStatement,
    factory,
    filter,
    findChildOfKind,
    flatMap,
    forEach,
    FunctionDeclaration,
    FunctionExpression,
    getEmitScriptTarget,
    getModeForUsageLocation,
    getQuotePreference,
    getResolvedModule,
    getSynthesizedDeepClone,
    getSynthesizedDeepClones,
    getSynthesizedDeepClonesWithReplacements,
    getSynthesizedDeepCloneWithReplacements,
    Identifier,
    ImportDeclaration,
    importFromModuleSpecifier,
    ImportSpecifier,
    InternalSymbolName,
    isArray,
    isArrowFunction,
    isBinaryExpression,
    isClassExpression,
    isExportsOrModuleExportsOrAlias,
    isFunctionExpression,
    isIdentifier,
    isIdentifierANonContextualKeyword,
    isObjectLiteralExpression,
    isPropertyAccessExpression,
    isRequireCall,
    isVariableStatement,
    makeImport,
    map,
    mapAllOrFail,
    mapIterator,
    MethodDeclaration,
    Modifier,
    Node,
    NodeArray,
    NodeFlags,
    ObjectLiteralElementLike,
    ObjectLiteralExpression,
    PropertyAccessExpression,
    QuotePreference,
    rangeContainsRange,
    ReadonlyCollection,
    ScriptTarget,
    some,
    SourceFile,
    Statement,
    StringLiteralLike,
    SymbolFlags,
    SyntaxKind,
    textChanges,
    TypeChecker,
    VariableStatement,
} from "../_namespaces/ts";
import {
    createCodeFixActionWithoutFixAll,
    moduleSpecifierToValidIdentifier,
    registerCodeFix,
} from "../_namespaces/ts.codefix";

registerCodeFix({
    errorCodes: [Diagnostics.File_is_a_CommonJS_module_it_may_be_converted_to_an_ES_module.code],
    getCodeActions(context) {
        const { sourceFile, program, preferences } = context;
        const changes = textChanges.ChangeTracker.with(context, changes => {
            const moduleExportsChangedToDefault = convertFileToEsModule(sourceFile, program.getTypeChecker(), changes, getEmitScriptTarget(program.getCompilerOptions()), getQuotePreference(sourceFile, preferences));
            if (moduleExportsChangedToDefault) {
                for (const importingFile of program.getSourceFiles()) {
                    fixImportOfModuleExports(importingFile, sourceFile, changes, getQuotePreference(importingFile, preferences));
                }
            }
        });
        // No support for fix-all since this applies to the whole file at once anyway.
        return [createCodeFixActionWithoutFixAll("convertToEsModule", changes, Diagnostics.Convert_to_ES_module)];
    },
});

function fixImportOfModuleExports(importingFile: SourceFile, exportingFile: SourceFile, changes: textChanges.ChangeTracker, quotePreference: QuotePreference) {
    for (const moduleSpecifier of importingFile.imports) {
        const imported = getResolvedModule(importingFile, moduleSpecifier.text, getModeForUsageLocation(importingFile, moduleSpecifier));
        if (!imported || imported.resolvedFileName !== exportingFile.fileName) {
            continue;
        }

        const importNode = importFromModuleSpecifier(moduleSpecifier);
        switch (importNode.kind) {
            case SyntaxKind.ImportEqualsDeclaration:
                changes.replaceNode(importingFile, importNode, makeImport(importNode.name, /*namedImports*/ undefined, moduleSpecifier, quotePreference));
                break;
            case SyntaxKind.CallExpression:
                if (isRequireCall(importNode, /*requireStringLiteralLikeArgument*/ false)) {
                    changes.replaceNode(importingFile, importNode, factory.createPropertyAccessExpression(getSynthesizedDeepClone(importNode), "default"));
                }
                break;
        }
    }
}

/** @returns Whether we converted a `module.exports =` to a default export. */
function convertFileToEsModule(sourceFile: SourceFile, checker: TypeChecker, changes: textChanges.ChangeTracker, target: ScriptTarget, quotePreference: QuotePreference): ModuleExportsChanged {
    const identifiers: Identifiers = { original: collectFreeIdentifiers(sourceFile), additional: new Set() };
    const exports = collectExportRenames(sourceFile, checker, identifiers);
    convertExportsAccesses(sourceFile, exports, changes);
    let moduleExportsChangedToDefault = false;
    let useSitesToUnqualify: Map<Node, Node> | undefined;
    // Process variable statements first to collect use sites that need to be updated inside other transformations
    for (const statement of filter(sourceFile.statements, isVariableStatement)) {
        const newUseSites = convertVariableStatement(sourceFile, statement, changes, checker, identifiers, target, quotePreference);
        if (newUseSites) {
            copyEntries(newUseSites, useSitesToUnqualify ??= new Map());
        }
    }
    // `convertStatement` will delete entries from `useSitesToUnqualify` when containing statements are replaced
    for (const statement of filter(sourceFile.statements, s => !isVariableStatement(s))) {
        const moduleExportsChanged = convertStatement(sourceFile, statement, checker, changes, identifiers, target, exports, useSitesToUnqualify, quotePreference);
        moduleExportsChangedToDefault = moduleExportsChangedToDefault || moduleExportsChanged;
    }
    // Remaining use sites can be changed directly
    useSitesToUnqualify?.forEach((replacement, original) => {
        changes.replaceNode(sourceFile, original, replacement);
    });

    return moduleExportsChangedToDefault;
}

/**
 * Contains an entry for each renamed export.
 * This is necessary because `exports.x = 0;` does not declare a local variable.
 * Converting this to `export const x = 0;` would declare a local, so we must be careful to avoid shadowing.
 * If there would be shadowing at either the declaration or at any reference to `exports.x` (now just `x`), we must convert to:
 *     const _x = 0;
 *     export { _x as x };
 * This conversion also must place if the exported name is not a valid identifier, e.g. `exports.class = 0;`.
 */
type ExportRenames = ReadonlyMap<string, string>;

function collectExportRenames(sourceFile: SourceFile, checker: TypeChecker, identifiers: Identifiers): ExportRenames {
    const res = new Map<string, string>();
    forEachExportReference(sourceFile, node => {
        const { text } = node.name;
        if (!res.has(text) && (isIdentifierANonContextualKeyword(node.name)
            || checker.resolveName(text, node, SymbolFlags.Value, /*excludeGlobals*/ true))) {
            // Unconditionally add an underscore in case `text` is a keyword.
            res.set(text, makeUniqueName(`_${text}`, identifiers));
        }
    });
    return res;
}

function convertExportsAccesses(sourceFile: SourceFile, exports: ExportRenames, changes: textChanges.ChangeTracker): void {
    forEachExportReference(sourceFile, (node, isAssignmentLhs) => {
        if (isAssignmentLhs) {
            return;
        }
        const { text } = node.name;
        changes.replaceNode(sourceFile, node, factory.createIdentifier(exports.get(text) || text));
    });
}

function forEachExportReference(sourceFile: SourceFile, cb: (node: (PropertyAccessExpression & { name: Identifier }), isAssignmentLhs: boolean) => void): void {
    sourceFile.forEachChild(function recur(node) {
        if (isPropertyAccessExpression(node) && isExportsOrModuleExportsOrAlias(sourceFile, node.expression) && isIdentifier(node.name)) {
            const { parent } = node;
            cb(node as typeof node & { name: Identifier }, isBinaryExpression(parent) && parent.left === node && parent.operatorToken.kind === SyntaxKind.EqualsToken);
        }
        node.forEachChild(recur);
    });
}

/** Whether `module.exports =` was changed to `export default` */
type ModuleExportsChanged = boolean;

/**
 * Converts a statement in a source file to a module export.
 * @param {SourceFile} sourceFile - The source file containing the statement.
 * @param {Statement} statement - The statement to convert.
 * @param {TypeChecker} checker - The type checker for the source file.
 * @param {textChanges.ChangeTracker} changes - The change tracker to record changes.
 * @param {Identifiers} identifiers - The identifiers in the source file.
 * @param {ScriptTarget} target - The target script version.
 * @param {ExportRenames} exports - The export renames.
 * @param {Map<Node, Node> | undefined} useSitesToUnqualify - The map of use sites to unqualify.
 * @param {QuotePreference} quotePreference - The quote preference for the module export.
 * @returns {boolean} - Whether the module exports have changed.
 */
function convertStatement(
    sourceFile: SourceFile,
    statement: Statement,
    checker: TypeChecker,
    changes: textChanges.ChangeTracker,
    identifiers: Identifiers,
    target: ScriptTarget,
    exports: ExportRenames,
    useSitesToUnqualify: Map<Node, Node> | undefined,
    quotePreference: QuotePreference
): ModuleExportsChanged {
    switch (statement.kind) {
        case SyntaxKind.VariableStatement:
            convertVariableStatement(sourceFile, statement as VariableStatement, changes, checker, identifiers, target, quotePreference);
            return false;
        case SyntaxKind.ExpressionStatement: {
            const { expression } = statement as ExpressionStatement;
            switch (expression.kind) {
                case SyntaxKind.CallExpression: {
                    if (isRequireCall(expression, /*requireStringLiteralLikeArgument*/ true)) {
                        // For side-effecting require() call, just make a side-effecting import.
                        changes.replaceNode(sourceFile, statement, makeImport(/*defaultImport*/ undefined, /*namedImports*/ undefined, expression.arguments[0], quotePreference));
                    }
                    return false;
                }
                case SyntaxKind.BinaryExpression: {
                    const { operatorToken } = expression as BinaryExpression;
                    return operatorToken.kind === SyntaxKind.EqualsToken && convertAssignment(sourceFile, checker, expression as BinaryExpression, changes, exports, useSitesToUnqualify);
                }
            }
        }
        // falls through
        default:
            return false;
    }
}

/**
 * Converts a VariableStatement to ES6 import statements and removes any unnecessary code.
 * @param sourceFile - The source file containing the VariableStatement.
 * @param statement - The VariableStatement to convert.
 * @param changes - The text changes to apply to the source file.
 * @param checker - The TypeChecker to use for type checking.
 * @param identifiers - The Identifiers to use for generating unique names.
 * @param target - The ScriptTarget to use for generating code.
 * @param quotePreference - The QuotePreference to use for string literals.
 * @returns A Map of nodes to their converted import statements, or undefined if no imports were found.
 */
function convertVariableStatement(
    sourceFile: SourceFile,
    statement: VariableStatement,
    changes: textChanges.ChangeTracker,
    checker: TypeChecker,
    identifiers: Identifiers,
    target: ScriptTarget,
    quotePreference: QuotePreference,
): Map<Node, Node> | undefined {
    const { declarationList } = statement;
    let foundImport = false;
    const converted = map(declarationList.declarations, decl => {
        const { name, initializer } = decl;
        if (initializer) {
            if (isExportsOrModuleExportsOrAlias(sourceFile, initializer)) {
                // `const alias = module.exports;` can be removed.
                foundImport = true;
                return convertedImports([]);
            }
            else if (isRequireCall(initializer, /*requireStringLiteralLikeArgument*/ true)) {
                foundImport = true;
                return convertSingleImport(name, initializer.arguments[0], checker, identifiers, target, quotePreference);
            }
            else if (isPropertyAccessExpression(initializer) && isRequireCall(initializer.expression, /*requireStringLiteralLikeArgument*/ true)) {
                foundImport = true;
                return convertPropertyAccessImport(name, initializer.name.text, initializer.expression.arguments[0], identifiers, quotePreference);
            }
        }
        // Move it out to its own variable statement. (This will not be used if `!foundImport`)
        return convertedImports([factory.createVariableStatement(/*modifiers*/ undefined, factory.createVariableDeclarationList([decl], declarationList.flags))]);
    });
    if (foundImport) {
        // useNonAdjustedEndPosition to ensure we don't eat the newline after the statement.
        changes.replaceNodeWithNodes(sourceFile, statement, flatMap(converted, c => c.newImports));
        let combinedUseSites: Map<Node, Node> | undefined;
        forEach(converted, c => {
            if (c.useSitesToUnqualify) {
                copyEntries(c.useSitesToUnqualify, combinedUseSites ??= new Map());
            }
        });

        return combinedUseSites;
    }
}

/**
 * Converts a property access import statement to an ES module import statement.
 * @param name The binding name of the import statement.
 * @param propertyName The name of the property being imported.
 * @param moduleSpecifier The module specifier of the import statement.
 * @param identifiers An object containing unique identifiers.
 * @param quotePreference The preferred quote style for the import statement.
 * @returns An object containing the converted imports.
 */
function convertPropertyAccessImport(name: BindingName, propertyName: string, moduleSpecifier: StringLiteralLike, identifiers: Identifiers, quotePreference: QuotePreference): ConvertedImports {
    switch (name.kind) {
        case SyntaxKind.ObjectBindingPattern:
        case SyntaxKind.ArrayBindingPattern: {
            // `const [a, b] = require("c").d` --> `import { d } from "c"; const [a, b] = d;`
            const tmp  = makeUniqueName(propertyName, identifiers);
            return convertedImports([
                makeSingleImport(tmp, propertyName, moduleSpecifier, quotePreference),
                makeConst(/*modifiers*/ undefined, name, factory.createIdentifier(tmp)),
            ]);
        }
        case SyntaxKind.Identifier:
            // `const a = require("b").c` --> `import { c as a } from "./b";
            return convertedImports([makeSingleImport(name.text, propertyName, moduleSpecifier, quotePreference)]);
        default:
            return Debug.assertNever(name, `Convert to ES module got invalid syntax form ${(name as BindingName).kind}`);
    }
}

function convertAssignment(
    sourceFile: SourceFile,
    checker: TypeChecker,
    assignment: BinaryExpression,
    changes: textChanges.ChangeTracker,
    exports: ExportRenames,
    useSitesToUnqualify: Map<Node, Node> | undefined,
): ModuleExportsChanged {
    const { left, right } = assignment;
    if (!isPropertyAccessExpression(left)) {
        return false;
    }

    if (isExportsOrModuleExportsOrAlias(sourceFile, left)) {
        if (isExportsOrModuleExportsOrAlias(sourceFile, right)) {
            // `const alias = module.exports;` or `module.exports = alias;` can be removed.
            changes.delete(sourceFile, assignment.parent);
        }
        else {
            const replacement = isObjectLiteralExpression(right) ? tryChangeModuleExportsObject(right, useSitesToUnqualify)
                : isRequireCall(right, /*requireStringLiteralLikeArgument*/ true) ? convertReExportAll(right.arguments[0], checker)
                : undefined;
            if (replacement) {
                changes.replaceNodeWithNodes(sourceFile, assignment.parent, replacement[0]);
                return replacement[1];
            }
            else {
                changes.replaceRangeWithText(sourceFile, createRange(left.getStart(sourceFile), right.pos), "export default");
                return true;
            }
        }
    }
    else if (isExportsOrModuleExportsOrAlias(sourceFile, left.expression)) {
        convertNamedExport(sourceFile, assignment as BinaryExpression & { left: PropertyAccessExpression }, changes, exports);
    }

    return false;
}

/**
 * Convert `module.exports = { ... }` to individual exports..
 * We can't always do this if the module has interesting members -- then it will be a default export instead.
 */
function tryChangeModuleExportsObject(object: ObjectLiteralExpression, useSitesToUnqualify: Map<Node, Node> | undefined): [readonly Statement[], ModuleExportsChanged] | undefined {
    const statements = mapAllOrFail(object.properties, prop => {
        switch (prop.kind) {
            case SyntaxKind.GetAccessor:
            case SyntaxKind.SetAccessor:
            // TODO: Maybe we should handle this? See fourslash test `refactorConvertToEs6Module_export_object_shorthand.ts`.
            // falls through
            case SyntaxKind.ShorthandPropertyAssignment:
            case SyntaxKind.SpreadAssignment:
                return undefined;
            case SyntaxKind.PropertyAssignment:
                return !isIdentifier(prop.name) ? undefined : convertExportsDotXEquals_replaceNode(prop.name.text, prop.initializer, useSitesToUnqualify);
            case SyntaxKind.MethodDeclaration:
                return !isIdentifier(prop.name) ? undefined : functionExpressionToDeclaration(prop.name.text, [factory.createToken(SyntaxKind.ExportKeyword)], prop, useSitesToUnqualify);
            default:
                Debug.assertNever(prop, `Convert to ES6 got invalid prop kind ${(prop as ObjectLiteralElementLike).kind}`);
        }
    });
    return statements && [statements, false];
}

/**
 * Converts a named export in a source file to a new name using a change tracker.
 * @param sourceFile - The source file containing the named export.
 * @param assignment - The binary expression representing the named export.
 * @param changes - The change tracker to use for making modifications.
 * @param exports - The export renames object containing the new names for exports.
 * @returns void
 */
function convertNamedExport(
    sourceFile: SourceFile,
    assignment: BinaryExpression & { left: PropertyAccessExpression },
    changes: textChanges.ChangeTracker,
    exports: ExportRenames,
): void {
    // If "originalKeywordKind" was set, this is e.g. `exports.
    const { text } = assignment.left.name;
    const rename = exports.get(text);
    if (rename !== undefined) {
        /*
        const _class = 0;
        export { _class as class };
        */
        const newNodes = [
            makeConst(/*modifiers*/ undefined, rename, assignment.right),
            makeExportDeclaration([factory.createExportSpecifier(/*isTypeOnly*/ false, rename, text)]),
        ];
        changes.replaceNodeWithNodes(sourceFile, assignment.parent, newNodes);
    }
    else {
        convertExportsPropertyAssignment(assignment, sourceFile, changes);
    }
}

/**
 * Converts a re-export of all exports from a module to the equivalent ES6 syntax.
 * @param {StringLiteralLike} reExported - The module specifier for the re-export statement.
 * @param {TypeChecker} checker - The type checker for the program.
 * @returns {[readonly Statement[], ModuleExportsChanged]} An array containing the transformed statements and a boolean indicating if the module exports have changed.
 */
function convertReExportAll(reExported: StringLiteralLike, checker: TypeChecker): [readonly Statement[], ModuleExportsChanged] {
    // `module.exports = require("x");` ==> `export * from "x"; export { default } from "x";`
    const moduleSpecifier = reExported.text;
    const moduleSymbol = checker.getSymbolAtLocation(reExported);
    const exports = moduleSymbol ? moduleSymbol.exports! : emptyMap as ReadonlyCollection<__String>;
    return exports.has(InternalSymbolName.ExportEquals) ? [[reExportDefault(moduleSpecifier)], true] :
        !exports.has(InternalSymbolName.Default) ? [[reExportStar(moduleSpecifier)], false] :
        // If there's some non-default export, must include both `export *` and `export default`.
        exports.size > 1 ? [[reExportStar(moduleSpecifier), reExportDefault(moduleSpecifier)], true] : [[reExportDefault(moduleSpecifier)], true];
}
function reExportStar(moduleSpecifier: string): ExportDeclaration {
    return makeExportDeclaration(/*exportSpecifiers*/ undefined, moduleSpecifier);
}
function reExportDefault(moduleSpecifier: string): ExportDeclaration {
    return makeExportDeclaration([factory.createExportSpecifier(/*isTypeOnly*/ false, /*propertyName*/ undefined, "default")], moduleSpecifier);
}

/**
 * Converts an `exports` property assignment to an `export` statement.
 * @param {object} param - An object containing the `left`, `right`, and `parent` properties.
 * @param {BinaryExpression & { left: PropertyAccessExpression }} param.left - The left side of the assignment.
 * @param {Node} param.right - The right side of the assignment.
 * @param {SourceFile} param.sourceFile - The source file being modified.
 * @param {textChanges.ChangeTracker} param.changes - The change tracker to use for modifying the source file.
 * @returns {void}
 */
function convertExportsPropertyAssignment({ left, right, parent }: BinaryExpression & { left: PropertyAccessExpression }, sourceFile: SourceFile, changes: textChanges.ChangeTracker): void {
    const name = left.name.text;
    if ((isFunctionExpression(right) || isArrowFunction(right) || isClassExpression(right)) && (!right.name || right.name.text === name)) {
        // `exports.f = function() {}` -> `export function f() {}` -- Replace `exports.f = ` with `export `, and insert the name after `function`.
        changes.replaceRange(sourceFile, { pos: left.getStart(sourceFile), end: right.getStart(sourceFile) }, factory.createToken(SyntaxKind.ExportKeyword), { suffix: " " });

        if (!right.name) changes.insertName(sourceFile, right, name);

        const semi = findChildOfKind(parent, SyntaxKind.SemicolonToken, sourceFile);
        if (semi) changes.delete(sourceFile, semi);
    }
    else {
        // `exports.f = function g() {}` -> `export const f = function g() {}` -- just replace `exports.` with `export const `
        changes.replaceNodeRangeWithNodes(sourceFile, left.expression, findChildOfKind(left, SyntaxKind.DotToken, sourceFile)!,
            [factory.createToken(SyntaxKind.ExportKeyword), factory.createToken(SyntaxKind.ConstKeyword)],
            { joiner: " ", suffix: " " });
    }
}

// TODO: GH#22492 this will cause an error if a change has been made inside the body of the node.
/**
 * Converts an exports assignment to an export statement.
 * @param {string | undefined} name - The name of the export.
 * @param {Expression} exported - The exported expression.
 * @param {Map<Node, Node> | undefined} useSitesToUnqualify - A map of nodes to their unqualified versions.
 * @returns {Statement} The converted export statement.
 */
function convertExportsDotXEquals_replaceNode(name: string | undefined, exported: Expression, useSitesToUnqualify: Map<Node, Node> | undefined): Statement {
    const modifiers = [factory.createToken(SyntaxKind.ExportKeyword)];
    switch (exported.kind) {
        case SyntaxKind.FunctionExpression: {
            const { name: expressionName } = exported as FunctionExpression;
            if (expressionName && expressionName.text !== name) {
                // `exports.f = function g() {}` -> `export const f = function g() {}`
                return exportConst();
            }
        }

        // falls through
        case SyntaxKind.ArrowFunction:
            // `exports.f = function() {}` --> `export function f() {}`
            return functionExpressionToDeclaration(name, modifiers, exported as FunctionExpression | ArrowFunction, useSitesToUnqualify);
        case SyntaxKind.ClassExpression:
            // `exports.C = class {}` --> `export class C {}`
            return classExpressionToDeclaration(name, modifiers, exported as ClassExpression, useSitesToUnqualify);
        default:
            return exportConst();
    }

    function exportConst() {
        // `exports.x = 0;` --> `export const x = 0;`
        return makeConst(modifiers, factory.createIdentifier(name!), replaceImportUseSites(exported, useSitesToUnqualify)); // TODO: GH#18217
    }
}

function replaceImportUseSites<T extends Node>(node: T, useSitesToUnqualify: Map<Node, Node> | undefined): T;
function replaceImportUseSites<T extends Node>(nodes: NodeArray<T>, useSitesToUnqualify: Map<Node, Node> | undefined): NodeArray<T>;
/**
 * Replaces import use sites with unqualified names.
 * @template T - The type of the node to replace.
 * @param {T | NodeArray<T>} nodeOrNodes - The node or nodes to replace.
 * @param {Map<Node, Node> | undefined} useSitesToUnqualify - The map of use sites to unqualify.
 * @returns {T | NodeArray<T>} - The replaced node or nodes.
 */
function replaceImportUseSites<T extends Node>(nodeOrNodes: T | NodeArray<T>, useSitesToUnqualify: Map<Node, Node> | undefined) {
    if (!useSitesToUnqualify || !some(arrayFrom(useSitesToUnqualify.keys()), original => rangeContainsRange(nodeOrNodes, original))) {
        return nodeOrNodes;
    }

    return isArray(nodeOrNodes)
        ? getSynthesizedDeepClonesWithReplacements(nodeOrNodes, /*includeTrivia*/ true, replaceNode)
        : getSynthesizedDeepCloneWithReplacements(nodeOrNodes, /*includeTrivia*/ true, replaceNode);

    function replaceNode(original: Node) {
        // We are replacing `mod.SomeExport` wih `SomeExport`, so we only need to look at PropertyAccessExpressions
        if (original.kind === SyntaxKind.PropertyAccessExpression) {
            const replacement = useSitesToUnqualify!.get(original);
            // Remove entry from `useSitesToUnqualify` so the refactor knows it's taken care of by the parent statement we're replacing
            useSitesToUnqualify!.delete(original);
            return replacement;
        }
    }
}

/**
 * Converts a single commonjs import statement to an ES module import statement.
 * @param {BindingName} name - The name of the imported variable.
 * @param {StringLiteralLike} moduleSpecifier - The module specifier of the imported module.
 * @param {TypeChecker} checker - The TypeChecker instance to use.
 * @param {Identifiers} identifiers - The Identifiers instance to use.
 * @param {ScriptTarget} target - The target script version.
 * @param {QuotePreference} quotePreference - The preferred quote style for strings.
 * @returns {ConvertedImports} An object containing the converted import statement.
 */
function convertSingleImport(
    name: BindingName,
    moduleSpecifier: StringLiteralLike,
    checker: TypeChecker,
    identifiers: Identifiers,
    target: ScriptTarget,
    quotePreference: QuotePreference,
): ConvertedImports {
    switch (name.kind) {
        case SyntaxKind.ObjectBindingPattern: {
            const importSpecifiers = mapAllOrFail(name.elements, e =>
                e.dotDotDotToken || e.initializer || e.propertyName && !isIdentifier(e.propertyName) || !isIdentifier(e.name)
                    ? undefined
                    : makeImportSpecifier(e.propertyName && e.propertyName.text, e.name.text));
            if (importSpecifiers) {
                return convertedImports([makeImport(/*defaultImport*/ undefined, importSpecifiers, moduleSpecifier, quotePreference)]);
            }
        }
        // falls through -- object destructuring has an interesting pattern and must be a variable declaration
        case SyntaxKind.ArrayBindingPattern: {
            /*
            import x from "x";
            const [a, b, c] = x;
            */
            const tmp = makeUniqueName(moduleSpecifierToValidIdentifier(moduleSpecifier.text, target), identifiers);
            return convertedImports([
                makeImport(factory.createIdentifier(tmp), /*namedImports*/ undefined, moduleSpecifier, quotePreference),
                makeConst(/*modifiers*/ undefined, getSynthesizedDeepClone(name), factory.createIdentifier(tmp)),
            ]);
        }
        case SyntaxKind.Identifier:
            return convertSingleIdentifierImport(name, moduleSpecifier, checker, identifiers, quotePreference);
        default:
            return Debug.assertNever(name, `Convert to ES module got invalid name kind ${(name as BindingName).kind}`);
    }
}

/**
 * Converts a single identifier import statement from CommonJS to ES6 syntax.
 *
 * @param name - The identifier to be imported.
 * @param moduleSpecifier - The module specifier string.
 * @param checker - The TypeChecker object.
 * @param identifiers - The Identifiers object.
 * @param quotePreference - The QuotePreference enum.
 *
 * @returns An object containing the converted import statement and any necessary updates to the code.
 */
function convertSingleIdentifierImport(name: Identifier, moduleSpecifier: StringLiteralLike, checker: TypeChecker, identifiers: Identifiers, quotePreference: QuotePreference): ConvertedImports {
    const nameSymbol = checker.getSymbolAtLocation(name);
    // Maps from module property name to name actually used. (The same if there isn't shadowing.)
    const namedBindingsNames = new Map<string, string>();
    // True if there is some non-property use like `x()` or `f(x)`.
    let needDefaultImport = false;
    let useSitesToUnqualify: Map<Node, Node> | undefined;

    for (const use of identifiers.original.get(name.text)!) {
        if (checker.getSymbolAtLocation(use) !== nameSymbol || use === name) {
            // This was a use of a different symbol with the same name, due to shadowing. Ignore.
            continue;
        }

        const { parent } = use;
        if (isPropertyAccessExpression(parent)) {
            const { name: { text: propertyName } } = parent;
            if (propertyName === "default") {
                needDefaultImport = true;

                const importDefaultName = use.getText();
                (useSitesToUnqualify ??= new Map()).set(parent, factory.createIdentifier(importDefaultName));
            }
            else {
                Debug.assert(parent.expression === use, "Didn't expect expression === use"); // Else shouldn't have been in `collectIdentifiers`
                let idName = namedBindingsNames.get(propertyName);
                if (idName === undefined) {
                    idName = makeUniqueName(propertyName, identifiers);
                    namedBindingsNames.set(propertyName, idName);
                }

                (useSitesToUnqualify ??= new Map()).set(parent, factory.createIdentifier(idName));
            }
        }
        else {
            needDefaultImport = true;
        }
    }

    const namedBindings = namedBindingsNames.size === 0 ? undefined : arrayFrom(mapIterator(namedBindingsNames.entries(), ([propertyName, idName]) =>
        factory.createImportSpecifier(/*isTypeOnly*/ false, propertyName === idName ? undefined : factory.createIdentifier(propertyName), factory.createIdentifier(idName))));
    if (!namedBindings) {
        // If it was unused, ensure that we at least import *something*.
        needDefaultImport = true;
    }
    return convertedImports(
        [makeImport(needDefaultImport ? getSynthesizedDeepClone(name) : undefined, namedBindings, moduleSpecifier, quotePreference)],
        useSitesToUnqualify
    );
}

// Identifiers helpers

function makeUniqueName(name: string, identifiers: Identifiers): string {
    while (identifiers.original.has(name) || identifiers.additional.has(name)) {
        name = `_${name}`;
    }
    identifiers.additional.add(name);
    return name;
}

/**
 * Helps us create unique identifiers.
 * `original` refers to the local variable names in the original source file.
 * `additional` is any new unique identifiers we've generated. (e.g., we'll generate `_x`, then `__x`.)
 */
interface Identifiers {
    readonly original: FreeIdentifiers;
    // Additional identifiers we've added. Mutable!
    readonly additional: Set<string>;
}

type FreeIdentifiers = ReadonlyMap<string, readonly Identifier[]>;
function collectFreeIdentifiers(file: SourceFile): FreeIdentifiers {
    const map = createMultiMap<string, Identifier>();
    forEachFreeIdentifier(file, id => map.add(id.text, id));
    return map;
}

/**
 * A free identifier is an identifier that can be accessed through name lookup as a local variable.
 * In the expression `x.y`, `x` is a free identifier, but `y` is not.
 */
function forEachFreeIdentifier(node: Node, cb: (id: Identifier) => void): void {
    if (isIdentifier(node) && isFreeIdentifier(node)) cb(node);
    node.forEachChild(child => forEachFreeIdentifier(child, cb));
}

/**
 * Determines if a given node is a free identifier.
 * @param node - The node to check.
 * @returns {boolean} - True if the node is a free identifier, false otherwise.
 */
function isFreeIdentifier(node: Identifier): boolean {
    const { parent } = node;
    switch (parent.kind) {
        case SyntaxKind.PropertyAccessExpression:
            return (parent as PropertyAccessExpression).name !== node;
        case SyntaxKind.BindingElement:
            return (parent as BindingElement).propertyName !== node;
        case SyntaxKind.ImportSpecifier:
            return (parent as ImportSpecifier).propertyName !== node;
        default:
            return true;
    }
}

// Node helpers

/**
 * Converts a function expression or arrow function to a function declaration.
 * @param name - The name of the function.
 * @param additionalModifiers - Additional modifiers for the function.
 * @param fn - The function expression, arrow function, or method declaration to convert.
 * @param useSitesToUnqualify - A map of nodes to their corresponding use sites for unqualifying imports.
 * @returns The converted function declaration.
 */
function functionExpressionToDeclaration(name: string | undefined, additionalModifiers: readonly Modifier[], fn: FunctionExpression | ArrowFunction | MethodDeclaration, useSitesToUnqualify: Map<Node, Node> | undefined): FunctionDeclaration {
    return factory.createFunctionDeclaration(
        concatenate(additionalModifiers, getSynthesizedDeepClones(fn.modifiers)),
        getSynthesizedDeepClone(fn.asteriskToken),
        name,
        getSynthesizedDeepClones(fn.typeParameters),
        getSynthesizedDeepClones(fn.parameters),
        getSynthesizedDeepClone(fn.type),
        factory.converters.convertToFunctionBlock(replaceImportUseSites(fn.body!, useSitesToUnqualify)));
}

function classExpressionToDeclaration(name: string | undefined, additionalModifiers: readonly Modifier[], cls: ClassExpression, useSitesToUnqualify: Map<Node, Node> | undefined): ClassDeclaration {
    return factory.createClassDeclaration(
        concatenate(additionalModifiers, getSynthesizedDeepClones(cls.modifiers)),
        name,
        getSynthesizedDeepClones(cls.typeParameters),
        getSynthesizedDeepClones(cls.heritageClauses),
        replaceImportUseSites(cls.members, useSitesToUnqualify));
}

function makeSingleImport(localName: string, propertyName: string, moduleSpecifier: StringLiteralLike, quotePreference: QuotePreference): ImportDeclaration {
    return propertyName === "default"
        ? makeImport(factory.createIdentifier(localName), /*namedImports*/ undefined, moduleSpecifier, quotePreference)
        : makeImport(/*defaultImport*/ undefined, [makeImportSpecifier(propertyName, localName)], moduleSpecifier, quotePreference);
}

function makeImportSpecifier(propertyName: string | undefined, name: string): ImportSpecifier {
    return factory.createImportSpecifier(/*isTypeOnly*/ false, propertyName !== undefined && propertyName !== name ? factory.createIdentifier(propertyName) : undefined, factory.createIdentifier(name));
}

function makeConst(modifiers: readonly Modifier[] | undefined, name: string | BindingName, init: Expression): VariableStatement {
    return factory.createVariableStatement(
        modifiers,
        factory.createVariableDeclarationList(
            [factory.createVariableDeclaration(name, /*exclamationToken*/ undefined, /*type*/ undefined, init)],
            NodeFlags.Const));
}

function makeExportDeclaration(exportSpecifiers: ExportSpecifier[] | undefined, moduleSpecifier?: string): ExportDeclaration {
    return factory.createExportDeclaration(
        /*modifiers*/ undefined,
        /*isTypeOnly*/ false,
        exportSpecifiers && factory.createNamedExports(exportSpecifiers),
        moduleSpecifier === undefined ? undefined : factory.createStringLiteral(moduleSpecifier));
}

interface ConvertedImports {
    newImports: readonly Node[];
    useSitesToUnqualify?: Map<Node, Node>;
}

function convertedImports(newImports: readonly Node[], useSitesToUnqualify?: Map<Node, Node>): ConvertedImports {
    return {
        newImports,
        useSitesToUnqualify
    };
}
