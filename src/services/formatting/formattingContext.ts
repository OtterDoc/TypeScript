import {
    Debug,
    findChildOfKind,
    FormatCodeSettings,
    Node,
    SourceFileLike,
    SyntaxKind,
} from "../_namespaces/ts";
import { TextRangeWithKind } from "../_namespaces/ts.formatting";

/** @internal */
export const enum FormattingRequestKind {
    FormatDocument,
    FormatSelection,
    FormatOnEnter,
    FormatOnSemicolon,
    FormatOnOpeningCurlyBrace,
    FormatOnClosingCurlyBrace
}

/**
 * Represents the formatting context for a source file.
 * @internal
 */
export class FormattingContext {
    public currentTokenSpan!: TextRangeWithKind;
    public nextTokenSpan!: TextRangeWithKind;
    public contextNode!: Node;
    public currentTokenParent!: Node;
    public nextTokenParent!: Node;

    private contextNodeAllOnSameLine: boolean | undefined;
    private nextNodeAllOnSameLine: boolean | undefined;
    private tokensAreOnSameLine: boolean | undefined;
    private contextNodeBlockIsOnOneLine: boolean | undefined;
    private nextNodeBlockIsOnOneLine: boolean | undefined;

    constructor(public readonly sourceFile: SourceFileLike, public formattingRequestKind: FormattingRequestKind, public options: FormatCodeSettings) {
    }

    /**
     * Updates the context of the current token and next token based on the provided parameters.
     * @param {TextRangeWithKind} currentRange - The current token range.
     * @param {Node} currentTokenParent - The parent node of the current token.
     * @param {TextRangeWithKind} nextRange - The next token range.
     * @param {Node} nextTokenParent - The parent node of the next token.
     * @param {Node} commonParent - The common parent node of the current and next tokens.
     */
    public updateContext(currentRange: TextRangeWithKind, currentTokenParent: Node, nextRange: TextRangeWithKind, nextTokenParent: Node, commonParent: Node) {
        this.currentTokenSpan = Debug.checkDefined(currentRange);
        this.currentTokenParent = Debug.checkDefined(currentTokenParent);
        this.nextTokenSpan = Debug.checkDefined(nextRange);
        this.nextTokenParent = Debug.checkDefined(nextTokenParent);
        this.contextNode = Debug.checkDefined(commonParent);

        // drop cached results
        this.contextNodeAllOnSameLine = undefined;
        this.nextNodeAllOnSameLine = undefined;
        this.tokensAreOnSameLine = undefined;
        this.contextNodeBlockIsOnOneLine = undefined;
        this.nextNodeBlockIsOnOneLine = undefined;
    }

    public ContextNodeAllOnSameLine(): boolean {
        if (this.contextNodeAllOnSameLine === undefined) {
            this.contextNodeAllOnSameLine = this.NodeIsOnOneLine(this.contextNode);
        }

        return this.contextNodeAllOnSameLine;
    }

    public NextNodeAllOnSameLine(): boolean {
        if (this.nextNodeAllOnSameLine === undefined) {
            this.nextNodeAllOnSameLine = this.NodeIsOnOneLine(this.nextTokenParent);
        }

        return this.nextNodeAllOnSameLine;
    }

    public TokensAreOnSameLine(): boolean {
        if (this.tokensAreOnSameLine === undefined) {
            const startLine = this.sourceFile.getLineAndCharacterOfPosition(this.currentTokenSpan.pos).line;
            const endLine = this.sourceFile.getLineAndCharacterOfPosition(this.nextTokenSpan.pos).line;
            this.tokensAreOnSameLine = (startLine === endLine);
        }

        return this.tokensAreOnSameLine;
    }

    public ContextNodeBlockIsOnOneLine() {
        if (this.contextNodeBlockIsOnOneLine === undefined) {
            this.contextNodeBlockIsOnOneLine = this.BlockIsOnOneLine(this.contextNode);
        }

        return this.contextNodeBlockIsOnOneLine;
    }

    public NextNodeBlockIsOnOneLine() {
        if (this.nextNodeBlockIsOnOneLine === undefined) {
            this.nextNodeBlockIsOnOneLine = this.BlockIsOnOneLine(this.nextTokenParent);
        }

        return this.nextNodeBlockIsOnOneLine;
    }

    private NodeIsOnOneLine(node: Node): boolean {
        const startLine = this.sourceFile.getLineAndCharacterOfPosition(node.getStart(this.sourceFile)).line;
        const endLine = this.sourceFile.getLineAndCharacterOfPosition(node.getEnd()).line;
        return startLine === endLine;
    }

    /**
     * Determines if a given node's block is on a single line.
     * @param {Node} node - The node to check.
     * @returns {boolean} - True if the block is on a single line, false otherwise.
     */
    private BlockIsOnOneLine(node: Node): boolean {
        const openBrace = findChildOfKind(node, SyntaxKind.OpenBraceToken, this.sourceFile);
        const closeBrace = findChildOfKind(node, SyntaxKind.CloseBraceToken, this.sourceFile);
        if (openBrace && closeBrace) {
            const startLine = this.sourceFile.getLineAndCharacterOfPosition(openBrace.getEnd()).line;
            const endLine = this.sourceFile.getLineAndCharacterOfPosition(closeBrace.getStart(this.sourceFile)).line;
            return startLine === endLine;
        }
        return false;
    }
}
