import {
    Diagnostics,
    factory,
    forEach,
    getSynthesizedDeepClone,
    getTokenAtPosition,
    hasJSDocNodes,
    InterfaceDeclaration,
    isJSDocTypedefTag,
    isJSDocTypeLiteral,
    JSDocPropertyLikeTag,
    JSDocTypedefTag,
    JSDocTypeExpression,
    JSDocTypeLiteral,
    mapDefined,
    Node,
    PropertySignature,
    some,
    SourceFile,
    SyntaxKind,
    textChanges,
    TypeAliasDeclaration,
} from "../_namespaces/ts";
import { codeFixAll, createCodeFixAction, registerCodeFix } from "../_namespaces/ts.codefix";

const fixId = "convertTypedefToType";
const errorCodes = [Diagnostics.JSDoc_typedef_may_be_converted_to_TypeScript_type.code];
registerCodeFix({
    fixIds: [fixId],
    errorCodes,
    /**
     * Retrieves code actions for converting a typedef to a TypeScript type.
     * @param {Context} context - The context object.
     * @returns {CodeAction[] | undefined} - An array of code fix actions if changes are found, undefined otherwise.
     */
    getCodeActions(context) {
        const node = getTokenAtPosition(
            context.sourceFile,
            context.span.start
        );
        if (!node) return;
        const changes = textChanges.ChangeTracker.with(context, t => doChange(t, node, context.sourceFile));

        if (changes.length > 0) {
            return [
                createCodeFixAction(
                    fixId,
                    changes,
                    Diagnostics.Convert_typedef_to_TypeScript_type,
                    fixId,
                    Diagnostics.Convert_all_typedef_to_TypeScript_types,
                ),
            ];
        }
    },
    getAllCodeActions: context => codeFixAll(context, errorCodes, (changes, diag) => {
        const node = getTokenAtPosition(diag.file, diag.start);
        if (node) doChange(changes, node, diag.file);
    })
});

function doChange(changes: textChanges.ChangeTracker, node: Node, sourceFile: SourceFile) {
    if (isJSDocTypedefTag(node)) {
        fixSingleTypeDef(changes, node, sourceFile);
    }
}

/**
 * Fixes a single type definition by replacing its JSDocTypedefTag with a new declaration.
 * @param {textChanges.ChangeTracker} changes - The change tracker to use for making modifications to the source file.
 * @param {JSDocTypedefTag | undefined} typeDefNode - The JSDocTypedefTag to fix.
 * @param {SourceFile} sourceFile - The source file containing the type definition.
 */
function fixSingleTypeDef(
    changes: textChanges.ChangeTracker,
    typeDefNode: JSDocTypedefTag | undefined,
    sourceFile: SourceFile,
) {
    if (!typeDefNode) return;

    const declaration = createDeclaration(typeDefNode);
    if (!declaration) return;

    const comment = typeDefNode.parent;

    changes.replaceNode(
        sourceFile,
        comment,
        declaration
    );
}

/**
 * Creates either an InterfaceDeclaration or a TypeAliasDeclaration based on the provided JSDocTypedefTag.
 * @param {JSDocTypedefTag} tag - The JSDocTypedefTag to create the declaration from.
 * @returns {InterfaceDeclaration | TypeAliasDeclaration | undefined} - Either an InterfaceDeclaration or a TypeAliasDeclaration, or undefined if the tag does not contain a typeExpression or a name.
 * @remarks - This function handles two use cases:
 * 1. When the typeExpression is a JSDocTypeLiteral, which can contain nested object types.
 * 2. When the typeExpression is a JSDocTypeExpression, which is the leaf node of the AST.
 */
function createDeclaration(tag: JSDocTypedefTag): InterfaceDeclaration | TypeAliasDeclaration | undefined {
    const { typeExpression } = tag;
    if (!typeExpression) return;
    const typeName = tag.name?.getText();
    if (!typeName) return;

    // For use case @typedef {object}Foo @property{bar}number
    // But object type can be nested, meaning the value in the k/v pair can be object itself
    if (typeExpression.kind === SyntaxKind.JSDocTypeLiteral) {
        return createInterfaceForTypeLiteral(typeName, typeExpression);
    }
    // for use case @typedef {(number|string|undefined)} Foo or @typedef {number|string|undefined} Foo
    // In this case, we reach the leaf node of AST.
    if (typeExpression.kind === SyntaxKind.JSDocTypeExpression) {
        return createTypeAliasForTypeExpression(typeName, typeExpression);
    }
}

/**
 * Creates an interface declaration for a given type literal.
 * @param {string} typeName - The name of the interface to be created.
 * @param {JSDocTypeLiteral} typeLiteral - The type literal to create the interface from.
 * @returns {InterfaceDeclaration | undefined} - The created interface declaration or undefined if no property signatures are found.
 */
function createInterfaceForTypeLiteral(
    typeName: string,
    typeLiteral: JSDocTypeLiteral
): InterfaceDeclaration | undefined {
    const propertySignatures = createSignatureFromTypeLiteral(typeLiteral);
    if (!some(propertySignatures)) return;
    const interfaceDeclaration = factory.createInterfaceDeclaration(
        /*modifiers*/ undefined,
        typeName,
        /*typeParameters*/ undefined,
        /*heritageClauses*/ undefined,
        propertySignatures,
    );
    return interfaceDeclaration;
}

/**
 * Creates a TypeAliasDeclaration based on the provided typeName and typeExpression.
 * @param {string} typeName - The name of the type alias to be created.
 * @param {JSDocTypeExpression} typeExpression - The type expression to be used as the type reference for the type alias.
 * @returns {TypeAliasDeclaration | undefined} - The created TypeAliasDeclaration or undefined if typeReference is not found.
 */
function createTypeAliasForTypeExpression(
    typeName: string,
    typeExpression: JSDocTypeExpression
): TypeAliasDeclaration | undefined {
    const typeReference = getSynthesizedDeepClone(typeExpression.type);
    if (!typeReference) return;
    const declaration = factory.createTypeAliasDeclaration(
        /*modifiers*/ undefined,
        factory.createIdentifier(typeName),
        /*typeParameters*/ undefined,
        typeReference
    );
    return declaration;
}

/**
 * Creates an array of PropertySignature objects from a JSDocTypeLiteral object.
 * @param {JSDocTypeLiteral} typeLiteral - The JSDocTypeLiteral object to create PropertySignature objects from.
 * @returns {PropertySignature[] | undefined} An array of PropertySignature objects or undefined if there are no property tags in the JSDocTypeLiteral object.
 */
function createSignatureFromTypeLiteral(typeLiteral: JSDocTypeLiteral): PropertySignature[] | undefined {
    const propertyTags = typeLiteral.jsDocPropertyTags;
    if (!some(propertyTags)) return;

    const getSignature = (tag: JSDocPropertyLikeTag) => {
        const name = getPropertyName(tag);
        const type = tag.typeExpression?.type;
        const isOptional = tag.isBracketed;
        let typeReference;

        // Recursively handle nested object type
        if (type && isJSDocTypeLiteral(type)) {
            const signatures = createSignatureFromTypeLiteral(type);
            typeReference = factory.createTypeLiteralNode(signatures);
        }
        // Leaf node, where type.kind === SyntaxKind.JSDocTypeExpression
        else if (type) {
            typeReference = getSynthesizedDeepClone(type);
        }

        if (typeReference && name) {
            const questionToken = isOptional ? factory.createToken(SyntaxKind.QuestionToken) : undefined;
            const prop = factory.createPropertySignature(
                /*modifiers*/ undefined,
                name,
                questionToken,
                typeReference
            );

            return prop;
        }
    };

    const props = mapDefined(propertyTags, getSignature);
    return props;
}

function getPropertyName(tag: JSDocPropertyLikeTag): string | undefined {
    return tag.name.kind === SyntaxKind.Identifier ? tag.name.text : tag.name.right.text;
}

/** @internal */
export function getJSDocTypedefNode(node: Node): JSDocTypedefTag | undefined {
    if (hasJSDocNodes(node)) {
        return forEach(node.jsDoc, (node) => node.tags?.find(isJSDocTypedefTag));
    }
    return undefined;
}
