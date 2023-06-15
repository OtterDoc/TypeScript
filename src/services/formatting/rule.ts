import {
    emptyArray,
    SyntaxKind,
} from "../_namespaces/ts";
import { FormattingContext } from "../_namespaces/ts.formatting";

/** @internal */
export interface Rule {
    // Used for debugging to identify each rule based on the property name it's assigned to.
    readonly debugName: string;
    readonly context: readonly ContextPredicate[];
    readonly action: RuleAction;
    readonly flags: RuleFlags;
}

/** @internal */
export type ContextPredicate = (context: FormattingContext) => boolean;
/** @internal */
export const anyContext: readonly ContextPredicate[] = emptyArray;

/**
 * Represents the possible actions that can be taken by a rule.
 * @enum {number}
 * @property {number} None - No action to be taken.
 * @property {number} StopProcessingSpaceActions - Stop processing space actions.
 * @property {number} StopProcessingTokenActions - Stop processing token actions.
 * @property {number} InsertSpace - Insert a space.
 * @property {number} InsertNewLine - Insert a new line.
 * @property {number} DeleteSpace - Delete a space.
 * @property {number} DeleteToken - Delete a token.
 * @property {number} InsertTrailingSemicolon - Insert a trailing semicolon.
 * @property {number} StopAction - Stop processing both space and token actions.
 * @property {number} ModifySpaceAction - Modify space actions.
 * @property {number} ModifyTokenAction - Modify token actions.
 */
export const enum RuleAction {
    None                       = 0,
    StopProcessingSpaceActions = 1 << 0,
    StopProcessingTokenActions = 1 << 1,
    InsertSpace                = 1 << 2,
    InsertNewLine              = 1 << 3,
    DeleteSpace                = 1 << 4,
    DeleteToken                = 1 << 5,
    InsertTrailingSemicolon    = 1 << 6,

    StopAction = StopProcessingSpaceActions | StopProcessingTokenActions,
    ModifySpaceAction = InsertSpace | InsertNewLine | DeleteSpace,
    ModifyTokenAction = DeleteToken | InsertTrailingSemicolon,
}

/** @internal */
export const enum RuleFlags {
    None,
    CanDeleteNewLines,
}

/** @internal */
export interface TokenRange {
    readonly tokens: readonly SyntaxKind[];
    readonly isSpecific: boolean;
}
