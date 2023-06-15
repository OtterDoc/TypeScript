import {
    __String,
    AccessExpression,
    ArrowFunction,
    BinaryExpression,
    Block,
    canHaveModifiers,
    ClassDeclaration,
    ClassElement,
    CodeFixContext,
    CompilerOptions,
    concatenate,
    copyLeadingComments,
    Diagnostics,
    every,
    Expression,
    factory,
    filter,
    forEach,
    FunctionDeclaration,
    FunctionExpression,
    getEmitScriptTarget,
    getNameOfDeclaration,
    getQuotePreference,
    getTokenAtPosition,
    idText,
    isAccessExpression,
    isArrowFunction,
    isBinaryExpression,
    isFunctionDeclaration,
    isFunctionExpression,
    isFunctionLike,
    isGetOrSetAccessorDeclaration,
    isIdentifier,
    isIdentifierText,
    isMethodDeclaration,
    isNoSubstitutionTemplateLiteral,
    isNumericLiteral,
    isObjectLiteralExpression,
    isPropertyAccessExpression,
    isPropertyAssignment,
    isSourceFileJS,
    isStringLiteralLike,
    isVariableDeclaration,
    isVariableDeclarationList,
    Modifier,
    Node,
    ObjectLiteralElementLike,
    ObjectLiteralExpression,
    PropertyAccessExpression,
    PropertyName,
    QuotePreference,
    some,
    SourceFile,
    Symbol,
    SymbolFlags,
    symbolName,
    SyntaxKind,
    textChanges,
    TypeChecker,
    UserPreferences,
    VariableDeclaration,
} from "../_namespaces/ts";
import {
    codeFixAll,
    createCodeFixAction,
    registerCodeFix,
} from "../_namespaces/ts.codefix";

const fixId = "convertFunctionToEs6Class";
const errorCodes = [Diagnostics.This_constructor_function_may_be_converted_to_a_class_declaration.code];
registerCodeFix({
    errorCodes,
    getCodeActions(context: CodeFixContext) {
        const changes = textChanges.ChangeTracker.with(context, t =>
            doChange(t, context.sourceFile, context.span.start, context.program.getTypeChecker(), context.preferences, context.program.getCompilerOptions()));
        return [createCodeFixAction(fixId, changes, Diagnostics.Convert_function_to_an_ES2015_class, fixId, Diagnostics.Convert_all_constructor_functions_to_classes)];
    },
    fixIds: [fixId],
    getAllCodeActions: context => codeFixAll(context, errorCodes, (changes, err) =>
        doChange(changes, err.file, err.start, context.program.getTypeChecker(), context.preferences, context.program.getCompilerOptions())),
});

function doChange(changes: textChanges.ChangeTracker, sourceFile: SourceFile, position: number, checker: TypeChecker, preferences: UserPreferences, compilerOptions: CompilerOptions): void {
    const ctorSymbol = checker.getSymbolAtLocation(getTokenAtPosition(sourceFile, position))!;
    if (!ctorSymbol || !ctorSymbol.valueDeclaration || !(ctorSymbol.flags & (SymbolFlags.Function | SymbolFlags.Variable))) {
        // Bad input
        return undefined;
    }

    const ctorDeclaration = ctorSymbol.valueDeclaration;
    if (isFunctionDeclaration(ctorDeclaration) || isFunctionExpression(ctorDeclaration)) {
        changes.replaceNode(sourceFile, ctorDeclaration, createClassFromFunction(ctorDeclaration));
    }
    else if (isVariableDeclaration(ctorDeclaration)) {
        const classDeclaration = createClassFromVariableDeclaration(ctorDeclaration);
        if (!classDeclaration) {
            return undefined;
        }

        const ancestor = ctorDeclaration.parent.parent;
        if (isVariableDeclarationList(ctorDeclaration.parent) && ctorDeclaration.parent.declarations.length > 1) {
            changes.delete(sourceFile, ctorDeclaration);
            changes.insertNodeAfter(sourceFile, ancestor, classDeclaration);
        }
        else {
            changes.replaceNode(sourceFile, ancestor, classDeclaration);
        }
    }

    function createClassElementsFromSymbol(symbol: Symbol) {
        const memberElements: ClassElement[] = [];
        // all static members are stored in the "exports" array of symbol
        if (symbol.exports) {
            symbol.exports.forEach(member => {
                if (member.name === "prototype" && member.declarations) {
                    const firstDeclaration = member.declarations[0];
                    // only one "x.prototype = { ... }" will pass
                    if (member.declarations.length === 1 &&
                        isPropertyAccessExpression(firstDeclaration) &&
                        isBinaryExpression(firstDeclaration.parent) &&
                        firstDeclaration.parent.operatorToken.kind === SyntaxKind.EqualsToken &&
                        isObjectLiteralExpression(firstDeclaration.parent.right)
                    ) {
                        const prototypes = firstDeclaration.parent.right;
                        createClassElement(prototypes.symbol, /*modifiers*/ undefined, memberElements);
                    }
                }
                else {
                    createClassElement(member, [factory.createToken(SyntaxKind.StaticKeyword)], memberElements);
                }
            });
        }

        // all instance members are stored in the "member" array of symbol (done last so instance members pulled from prototype assignments have priority)
        if (symbol.members) {
            symbol.members.forEach((member, key) => {
                if (key === "constructor" && member.valueDeclaration) {
                    const prototypeAssignment = symbol.exports?.get("prototype" as __String)?.declarations?.[0]?.parent;
                    if (prototypeAssignment && isBinaryExpression(prototypeAssignment) && isObjectLiteralExpression(prototypeAssignment.right) && some(prototypeAssignment.right.properties, isConstructorAssignment)) {
                        // fn.prototype = { constructor: fn }
                        // Already deleted in `createClassElement` in first pass
                    }
                    else {
                        // fn.prototype.constructor = fn
                        changes.delete(sourceFile, member.valueDeclaration.parent);
                    }
                    return;
                }
                createClassElement(member, /*modifiers*/ undefined, memberElements);
            });
        }

        return memberElements;

        /**
         * Determines if a given target and source should be converted based on their types.
         * @param _target - The target to check for conversion.
         * @param source - The source to check for conversion.
         * @returns {boolean} - True if the target and source should be converted, false otherwise.
         * @remarks - Right now only function expressions, get/set accessors, and methods can be converted. Other values like normal value fields ({a: 1}) should not be transformed. This can be updated once ES public class properties are available.
         */
        function shouldConvertDeclaration(_target: AccessExpression | ObjectLiteralExpression, source: Expression) {
            // Right now the only thing we can convert are function expressions, get/set accessors and methods
            // other values like normal value fields ({a: 1}) shouldn't get transformed.
            // We can update this once ES public class properties are available.
            if (isAccessExpression(_target)) {
                if (isPropertyAccessExpression(_target) && isConstructorAssignment(_target)) return true;
                return isFunctionLike(source);
            }
            else {
                return every(_target.properties, property => {
                    // a() {}
                    if (isMethodDeclaration(property) || isGetOrSetAccessorDeclaration(property)) return true;
                    // a: function() {}
                    if (isPropertyAssignment(property) && isFunctionExpression(property.initializer) && !!property.name) return true;
                    // x.prototype.constructor = fn
                    if (isConstructorAssignment(property)) return true;
                    return false;
                });
            }
        }

        /**
         * Converts a symbol to a class element and adds it to the members array.
         * @param {Symbol} symbol - The symbol to convert.
         * @param {Modifier[] | undefined} modifiers - The modifiers to apply to the converted element.
         * @param {ClassElement[]} members - The array to add the converted element to.
         * @returns {void}
         */
        function createClassElement(symbol: Symbol, modifiers: Modifier[] | undefined, members: ClassElement[]): void {
            // Right now the only thing we can convert are function expressions, which are marked as methods
            // or { x: y } type prototype assignments, which are marked as ObjectLiteral
            if (!(symbol.flags & SymbolFlags.Method) && !(symbol.flags & SymbolFlags.ObjectLiteral)) {
                return;
            }

            const memberDeclaration = symbol.valueDeclaration as AccessExpression | ObjectLiteralExpression;
            const assignmentBinaryExpression = memberDeclaration.parent as BinaryExpression;
            const assignmentExpr = assignmentBinaryExpression.right;
            if (!shouldConvertDeclaration(memberDeclaration, assignmentExpr)) {
                return;
            }

            if (some(members, m => {
                const name = getNameOfDeclaration(m);
                if (name && isIdentifier(name) && idText(name) === symbolName(symbol)) {
                    return true; // class member already made for this name
                }
                return false;
            })) {
                return;
            }

            // delete the entire statement if this expression is the sole expression to take care of the semicolon at the end
            const nodeToDelete = assignmentBinaryExpression.parent && assignmentBinaryExpression.parent.kind === SyntaxKind.ExpressionStatement
                ? assignmentBinaryExpression.parent : assignmentBinaryExpression;
            changes.delete(sourceFile, nodeToDelete);

            if (!assignmentExpr) {
                members.push(factory.createPropertyDeclaration(modifiers, symbol.name, /*questionOrExclamationToken*/ undefined,
                    /*type*/ undefined, /*initializer*/ undefined));
                return;
            }

            // f.x = expr
            if (isAccessExpression(memberDeclaration) && (isFunctionExpression(assignmentExpr) || isArrowFunction(assignmentExpr))) {
                const quotePreference = getQuotePreference(sourceFile, preferences);
                const name = tryGetPropertyName(memberDeclaration, compilerOptions, quotePreference);
                if (name) {
                    createFunctionLikeExpressionMember(members, assignmentExpr, name);
                }
                return;
            }
            // f.prototype = { ... }
            else if (isObjectLiteralExpression(assignmentExpr)) {
                forEach(
                    assignmentExpr.properties,
                    property => {
                        if (isMethodDeclaration(property) || isGetOrSetAccessorDeclaration(property)) {
                            // MethodDeclaration and AccessorDeclaration can appear in a class directly
                            members.push(property);
                        }
                        if (isPropertyAssignment(property) && isFunctionExpression(property.initializer)) {
                            createFunctionLikeExpressionMember(members, property.initializer, property.name);
                        }
                        // Drop constructor assignments
                        if (isConstructorAssignment(property)) return;
                        return;
                    }
                );
                return;
            }
            else {
                // Don't try to declare members in JavaScript files
                if (isSourceFileJS(sourceFile)) return;
                if (!isPropertyAccessExpression(memberDeclaration)) return;
                const prop = factory.createPropertyDeclaration(modifiers, memberDeclaration.name, /*questionOrExclamationToken*/ undefined, /*type*/ undefined, assignmentExpr);
                copyLeadingComments(assignmentBinaryExpression.parent, prop, sourceFile);
                members.push(prop);
                return;
            }

            function createFunctionLikeExpressionMember(members: ClassElement[], expression: FunctionExpression | ArrowFunction, name: PropertyName) {
                if (isFunctionExpression(expression)) return createFunctionExpressionMember(members, expression, name);
                else return createArrowFunctionExpressionMember(members, expression, name);
            }

            function createFunctionExpressionMember(members: ClassElement[], functionExpression: FunctionExpression, name: PropertyName) {
                const fullModifiers = concatenate(modifiers, getModifierKindFromSource(functionExpression, SyntaxKind.AsyncKeyword));
                const method = factory.createMethodDeclaration(fullModifiers, /*asteriskToken*/ undefined, name, /*questionToken*/ undefined,
                    /*typeParameters*/ undefined, functionExpression.parameters, /*type*/ undefined, functionExpression.body);
                copyLeadingComments(assignmentBinaryExpression, method, sourceFile);
                members.push(method);
                return;
            }

            /**
             * Creates an arrow function expression member for a class.
             * @param members - An array of ClassElement objects.
             * @param arrowFunction - The arrow function to be converted to a method.
             * @param name - The name of the method.
             * @returns void
             */
            function createArrowFunctionExpressionMember(members: ClassElement[], arrowFunction: ArrowFunction, name: PropertyName) {
                const arrowFunctionBody = arrowFunction.body;
                let bodyBlock: Block;

                // case 1: () => { return [1,2,3] }
                if (arrowFunctionBody.kind === SyntaxKind.Block) {
                    bodyBlock = arrowFunctionBody as Block;
                }
                // case 2: () => [1,2,3]
                else {
                    bodyBlock = factory.createBlock([factory.createReturnStatement(arrowFunctionBody)]);
                }
                const fullModifiers = concatenate(modifiers, getModifierKindFromSource(arrowFunction, SyntaxKind.AsyncKeyword));
                const method = factory.createMethodDeclaration(fullModifiers, /*asteriskToken*/ undefined, name, /*questionToken*/ undefined,
                    /*typeParameters*/ undefined, arrowFunction.parameters, /*type*/ undefined, bodyBlock);
                copyLeadingComments(assignmentBinaryExpression, method, sourceFile);
                members.push(method);
            }
        }
    }

    /**
     * Creates a ClassDeclaration from a VariableDeclaration node.
     * @param node - The VariableDeclaration node to create the class from.
     * @returns The created ClassDeclaration or undefined if the node is not valid.
     */
    function createClassFromVariableDeclaration(node: VariableDeclaration): ClassDeclaration | undefined {
        const initializer = node.initializer;
        if (!initializer || !isFunctionExpression(initializer) || !isIdentifier(node.name)) {
            return undefined;
        }

        const memberElements = createClassElementsFromSymbol(node.symbol);
        if (initializer.body) {
            memberElements.unshift(factory.createConstructorDeclaration(/*modifiers*/ undefined, initializer.parameters, initializer.body));
        }

        const modifiers = getModifierKindFromSource(node.parent.parent, SyntaxKind.ExportKeyword);
        const cls = factory.createClassDeclaration(modifiers, node.name,
            /*typeParameters*/ undefined, /*heritageClauses*/ undefined, memberElements);
        // Don't call copyComments here because we'll already leave them in place
        return cls;
    }

    /**
     * Creates a ClassDeclaration from a FunctionDeclaration or FunctionExpression node.
     * @param node The node to create the class from.
     * @returns The created ClassDeclaration.
     */
    function createClassFromFunction(node: FunctionDeclaration | FunctionExpression): ClassDeclaration {
        const memberElements = createClassElementsFromSymbol(ctorSymbol);
        if (node.body) {
            memberElements.unshift(factory.createConstructorDeclaration(/*modifiers*/ undefined, node.parameters, node.body));
        }

        const modifiers = getModifierKindFromSource(node, SyntaxKind.ExportKeyword);
        const cls = factory.createClassDeclaration(modifiers, node.name,
            /*typeParameters*/ undefined, /*heritageClauses*/ undefined, memberElements);
        // Don't call copyComments here because we'll already leave them in place
        return cls;
    }
}

function getModifierKindFromSource(source: Node, kind: Modifier["kind"]): readonly Modifier[] | undefined {
    return canHaveModifiers(source) ? filter(source.modifiers, (modifier): modifier is Modifier => modifier.kind === kind) : undefined;
}

function isConstructorAssignment(x: ObjectLiteralElementLike | PropertyAccessExpression) {
    if (!x.name) return false;
    if (isIdentifier(x.name) && x.name.text === "constructor") return true;
    return false;
}

/**
 * Tries to get the property name from an AccessExpression node.
 * @param {AccessExpression} node - The AccessExpression node to get the property name from.
 * @param {CompilerOptions} compilerOptions - The compiler options to use.
 * @param {QuotePreference} quotePreference - The quote preference to use.
 * @returns {PropertyName | undefined} The property name if found, otherwise undefined.
 */
function tryGetPropertyName(node: AccessExpression, compilerOptions: CompilerOptions, quotePreference: QuotePreference): PropertyName | undefined {
    if (isPropertyAccessExpression(node)) {
        return node.name;
    }

    const propName = node.argumentExpression;
    if (isNumericLiteral(propName)) {
        return propName;
    }

    if (isStringLiteralLike(propName)) {
        return isIdentifierText(propName.text, getEmitScriptTarget(compilerOptions)) ? factory.createIdentifier(propName.text)
            : isNoSubstitutionTemplateLiteral(propName) ? factory.createStringLiteral(propName.text, quotePreference === QuotePreference.Single)
            : propName;
    }

    return undefined;
}
