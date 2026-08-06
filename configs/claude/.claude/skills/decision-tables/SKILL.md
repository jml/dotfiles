---
name: decision-tables
description: Generate decision tables in the style of Hillel Wayne. Use when the user asks for a decision table, or when mapping out conditional logic — branching specs, tangled if/else code, "what happens when X and Y" questions over multiple boolean or enumerated inputs.
---

# Decision Tables

## Overview

A decision table represents branching logic as a table: one column per input, one column per output, one row per case. Its value is exhaustiveness — building one forces every combination of inputs to be considered, which surfaces the cases nobody decided.

Sources: [Decision Tables](https://www.hillelwayne.com/post/decision-tables/) and [Decision Table Patterns](https://www.hillelwayne.com/post/decision-table-patterns/) by Hillel Wayne.

A table is **valid** when it is both:

- **Complete** — every possible input combination is covered by some row.
- **Sound** — no input combination is covered by two rows with different outputs.

Producing a valid table is the exercise. Rows the source material doesn't specify are the payoff, not an obstacle.

## When to Use

- User asks for a decision table.
- A spec, design doc, or discussion has 2+ interacting conditions and the branching lacks an obvious pattern.
- Reviewing or refactoring nested/tangled conditionals.
- The problem decomposes into discrete, independent inputs. If it doesn't (sequences, deep data structures, behaviour over time), say so and suggest a state machine or flowchart instead.

## Construction

1. **Choose inputs.** Prefer booleans and small enums. Collapse continuous values into intervals (equivalence classes). Fold dependent booleans into one enum: `form_exists`/`submitted` where the second is meaningless without the first becomes `form_status ∈ {not_started, incomplete, submitted}`.
2. **Choose outputs.** Results, effects, or new state. Multiple output columns are fine.
3. **Enumerate every input combination.** Start exhaustive; compress afterwards.
4. **Fill in outputs from the source material only.** Where the spec/code/conversation doesn't determine an output, write `???`. Never invent a plausible output for an unspecified row — those rows are the questions to bring back to the user.
5. **Compress** with don't-cares and enum folding (see Notation and Patterns).
6. **Verify validity** (see below) before presenting.

## Notation

| Symbol | Meaning |
|--------|---------|
| `T` / `F` | Boolean values |
| `-` | Don't-care: output is the same for any value of this input |
| `/` | Impossible combination (logically cannot occur) |
| `???` | Unspecified: the source material doesn't say |
| `[a, b)` | Interval, inclusive `[`/exclusive `)`; omitted bound = infinity |
| `x` / `x'` | Value before / after the step, for state mutations |

- Distinguish **facts** from **assertions**. `/` is for combinations that are logically impossible (2FA response can't be correct when 2FA is disabled). For combinations that *shouldn't* happen but could (invariant violations), keep the row and give it an error output or `???` — the reader should decide what happens when the invariant breaks.
- Give a legend for every non-boolean input: `status ∈ {active, suspended, deleted}`. Without the legend, completeness can't be checked.
- Rows are order-independent. Never rely on first-match-wins semantics — if two rows overlap, the table is unsound; fix the rows.

## Verifying validity

Do this mechanically before presenting any table:

- **Completeness:** each row covers `Π(cells)` combinations, where a specific value counts 1 and `-` counts the domain size. The rows must sum to the product of all input domain sizes. Note the tally under the table when don't-cares are involved (e.g. "covers 16/16 combinations").
- **Soundness:** don't-cares overlap easily — `| T | - |` and `| - | T |` both cover `(T, T)`. Check each pair of rows: they must differ in at least one column where both have specific, unequal values.
- `/` rows count toward completeness (they cover their combinations; the coverage is the claim that they're unreachable).

## Patterns

- **Don't-care compression:** merge rows that differ only in inputs the output ignores.
- **Input elimination:** if a column is `-` in every row after compression, the input is irrelevant — remove it and say so. This is a finding worth reporting.
- **Table decomposition:** when different outputs depend on disjoint inputs, split into one table per output. Skip it when the outputs are conceptually one decision — smaller isn't clearer if the reader must join tables back together.
- **Interval alignment:** when rows partition a numeric range differently depending on another input, re-partition using the superset of all boundaries so every row uses the same intervals. Watch the endpoints when translating.
- **State mutation:** one table = one step. `x'` never appears in an input column. For multi-step logic, chain tables or switch to a state machine.

## Formatting

- Sort rows like a multi-column sort, leftmost input most significant, consistent value order within a column (e.g. `T` before `F`). The reader should be able to binary-search for a case.
- Order input columns by reasoning order: the ones that decide the most, leftmost; columns full of `-`, rightmost.
- Inputs left, outputs right. If it helps, note which is which above the table.
- A table that doesn't fit on a screen is a smell: decompose, fold enums, or reconsider the inputs.

## Presenting results

- Lead with the table, then the findings: `???` rows, `/` rows the reader may disagree are impossible, eliminated inputs, and any unsoundness found in the *source* logic (e.g. two code paths claim the same case).
- Frame `???` rows as direct questions: "What happens when the override points at a manual upload?"
- If asked to turn the table into tests: one test per row, expanding don't-cares only when the implementation might distinguish them.

## Example

`n` is a positive integer.

| `n % 3 == 0` | `n % 5 == 0` | output |
|:---:|:---:|---|
| T | T | "FizzBuzz" |
| T | F | "Fizz" |
| F | T | "Buzz" |
| F | F | `n` |

Covers 4/4 combinations; no overlaps.

## Common Mistakes

| Mistake | Fix |
|---------|-----|
| Inventing outputs for unspecified rows | Mark `???` and ask. The gaps are the deliverable. |
| Overlapping don't-cares | Pairwise soundness check before presenting. |
| Silently dropping "can't happen" rows | Keep them as `/` (fact) or error/`???` (assertion). |
| No legend for enum inputs | Legend every non-boolean column. |
| Rows in discovery order | Sort by inputs, leftmost most significant. |
| One giant table for temporal or structured logic | Decompose, or recommend a state machine/flowchart. |
