import {
    Debug,
    every,
    FormatCodeSettings,
    FormattingHost,
    SyntaxKind,
} from "../_namespaces/ts";
import {
    anyContext,
    FormatContext,
    FormattingContext,
    getAllRules,
    Rule,
    RuleAction,
    RuleSpec,
} from "../_namespaces/ts.formatting";

/** @internal */
export function getFormatContext(options: FormatCodeSettings, host: FormattingHost): FormatContext {
    return { options, getRules: getRulesMap(), host };
}

let rulesMapCache: RulesMap | undefined;

function getRulesMap(): RulesMap {
    if (rulesMapCache === undefined) {
        rulesMapCache = createRulesMap(getAllRules());
    }
    return rulesMapCache;
}

/**
 * Returns a mask of other rule actions that cannot be applied at the same position as the given rule action.
 * @param {RuleAction} ruleAction - The rule action to check against.
 * @returns {RuleAction} - A mask of other rule actions that cannot be applied at the same position.
 */
function getRuleActionExclusion(ruleAction: RuleAction): RuleAction {
    let mask = RuleAction.None;
    if (ruleAction & RuleAction.StopProcessingSpaceActions) {
        mask |= RuleAction.ModifySpaceAction;
    }
    if (ruleAction & RuleAction.StopProcessingTokenActions) {
        mask |= RuleAction.ModifyTokenAction;
    }
    if (ruleAction & RuleAction.ModifySpaceAction) {
        mask |= RuleAction.ModifySpaceAction;
    }
    if (ruleAction & RuleAction.ModifyTokenAction) {
        mask |= RuleAction.ModifyTokenAction;
    }
    return mask;
}

/** @internal */
export type RulesMap = (context: FormattingContext) => readonly Rule[] | undefined;
/**
 * Creates a mapping of rules based on the provided RuleSpec array.
 * @param {readonly RuleSpec[]} rules - An array of RuleSpec objects.
 * @returns {RulesMap} A function that takes a context object and returns an array of Rule objects.
 */
function createRulesMap(rules: readonly RuleSpec[]): RulesMap {
    const map = buildMap(rules);
    return context => {
        const bucket = map[getRuleBucketIndex(context.currentTokenSpan.kind, context.nextTokenSpan.kind)];
        if (bucket) {
            const rules: Rule[] = [];
            let ruleActionMask: RuleAction = 0;
            for (const rule of bucket) {
                const acceptRuleActions = ~getRuleActionExclusion(ruleActionMask);
                if (rule.action & acceptRuleActions && every(rule.context, c => c(context))) {
                    rules.push(rule);
                    ruleActionMask |= rule.action;
                }
            }
            if (rules.length) {
                return rules;
            }
        }
    };
}

/**
 * Builds a map of rules based on an array of RuleSpec objects.
 * @param {readonly RuleSpec[]} rules - The array of RuleSpec objects to build the map from.
 * @returns {readonly (readonly Rule[])[]} An array of arrays of Rule objects representing the map.
 * The outer array represents the rows of the map, while the inner arrays represent the columns.
 * Each Rule object represents a rule that applies to a specific combination of left and right tokens.
 * @remarks This function uses a bucketing strategy to group rules that apply to the same combination of tokens.
 * The bucket index is calculated using the getRuleBucketIndex function.
 * During construction of the rules bucket in the map, the rulesBucketConstructionStateList array is used to keep track of the state of each bucket.
 * This array is not used after construction is complete.
 */
function buildMap(rules: readonly RuleSpec[]): readonly (readonly Rule[])[] {
    // Map from bucket index to array of rules
    const map: Rule[][] = new Array(mapRowLength * mapRowLength);
    // This array is used only during construction of the rulesbucket in the map
    const rulesBucketConstructionStateList = new Array<number>(map.length);
    for (const rule of rules) {
        const specificRule = rule.leftTokenRange.isSpecific && rule.rightTokenRange.isSpecific;

        for (const left of rule.leftTokenRange.tokens) {
            for (const right of rule.rightTokenRange.tokens) {
                const index = getRuleBucketIndex(left, right);
                let rulesBucket = map[index];
                if (rulesBucket === undefined) {
                    rulesBucket = map[index] = [];
                }
                addRule(rulesBucket, rule.rule, specificRule, rulesBucketConstructionStateList, index);
            }
        }
    }
    return map;
}

function getRuleBucketIndex(row: number, column: number): number {
    Debug.assert(row <= SyntaxKind.LastKeyword && column <= SyntaxKind.LastKeyword, "Must compute formatting context from tokens");
    return (row * mapRowLength) + column;
}

const maskBitSize = 5;
const mask = 0b11111; // MaskBitSize bits
const mapRowLength = SyntaxKind.LastToken + 1;

enum RulesPosition {
    StopRulesSpecific = 0,
    StopRulesAny = maskBitSize * 1,
    ContextRulesSpecific = maskBitSize * 2,
    ContextRulesAny = maskBitSize * 3,
    NoContextRulesSpecific = maskBitSize * 4,
    NoContextRulesAny = maskBitSize * 5
}

// The Rules list contains all the inserted rules into a rulebucket in the following order:
//    1- Ignore rules with specific token combination
//    2- Ignore rules with any token combination
//    3- Context rules with specific token combination
//    4- Context rules with any token combination
//    5- Non-context rules with specific token combination
//    6- Non-context rules with any token combination
//
// The member rulesInsertionIndexBitmap is used to describe the number of rules
// in each sub-bucket (above) hence can be used to know the index of where to insert
// the next rule. It's a bitmap which contains 6 different sections each is given 5 bits.
//
// Example:
// In order to insert a rule to the end of sub-bucket (3), we get the index by adding
// the values in the bitmap segments 3rd, 2nd, and 1st.
/**
 * Adds a rule to an array of rules.
 * @param {Rule[]} rules - The array of rules to add the new rule to.
 * @param {Rule} rule - The new rule to add to the array.
 * @param {boolean} specificTokens - A boolean indicating whether the rule applies to specific tokens.
 * @param {number[]} constructionState - An array of numbers representing the construction state.
 * @param {number} rulesBucketIndex - The index of the rules bucket.
 * @returns {void}
 */
function addRule(rules: Rule[], rule: Rule, specificTokens: boolean, constructionState: number[], rulesBucketIndex: number): void {
    const position = rule.action & RuleAction.StopAction ?
        specificTokens ? RulesPosition.StopRulesSpecific : RulesPosition.StopRulesAny :
        rule.context !== anyContext ?
            specificTokens ? RulesPosition.ContextRulesSpecific : RulesPosition.ContextRulesAny :
            specificTokens ? RulesPosition.NoContextRulesSpecific : RulesPosition.NoContextRulesAny;

    const state = constructionState[rulesBucketIndex] || 0;
    rules.splice(getInsertionIndex(state, position), 0, rule);
    constructionState[rulesBucketIndex] = increaseInsertionIndex(state, position);
}

function getInsertionIndex(indexBitmap: number, maskPosition: RulesPosition) {
    let index = 0;
    for (let pos = 0; pos <= maskPosition; pos += maskBitSize) {
        index += indexBitmap & mask;
        indexBitmap >>= maskBitSize;
    }
    return index;
}

function increaseInsertionIndex(indexBitmap: number, maskPosition: RulesPosition): number {
    const value = ((indexBitmap >> maskPosition) & mask) + 1;
    Debug.assert((value & mask) === value, "Adding more rules into the sub-bucket than allowed. Maximum allowed is 32 rules.");
    return (indexBitmap & ~(mask << maskPosition)) | (value << maskPosition);
}
