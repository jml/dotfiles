---
name: design-doc-prose-review
description: Reviews the prose of a design doc, RFC, intent doc, or 1-pager against jml's long-form writing rules and reports findings with line, quote, rule, and a proposed rewrite. Use when asked to "review the prose", do a "prose pass", check whether a doc "reads as AI-written" or "slop", tighten or edit a design doc, or right after drafting one. Covers fact routing across sections and docs, argument discipline, punchline and coinage rationing, temporal frames, verb-driven sentences, and grep-able AI-texture tells.
---

# Design Doc Prose Review

Use this skill to review the prose of long-form technical documents: design
docs, RFCs, intent docs, 1-pagers, and READMEs. It does not review code
comments (use `code-comments`) or PR descriptions (the global CLAUDE.md "Pull
Requests" rules cover those).

Always run this review as its own pass, separate from drafting, even on a doc
you just wrote. The same model that reliably produces these failures while
generating reliably catches them while reviewing, even with the rules loaded,
so a drafting pass that "followed the rules" has not been reviewed.

## Sources

The rules come from jml's hand-edits and prose reviews of the dataplane
dual-write and elastic-build docs (2026-07-30/31), the dataplane
developer-access intent doc (2026-09-07), the GetTar comment session
(2026-08-03), and an ecosystems 1-pager (2026-08-05). They are consolidated in
the two reference files:

- [Structure and argument rules](references/structure-and-argument.md): S1–S9
- [Sentence and texture rules](references/sentence-and-texture.md): T1–T16

This skill is the only home for these rules. When a review, or the user's
edit of a draft, teaches a new rule, add it to the matching reference file with
a wrong and a correct example and an audit line, and add it to the rule index
below. Never record it in a memory note or a separate style file.

## Procedure

1. **Name the genre.** Decide whether the doc is a design doc, intent doc,
   1-pager, or README, and state it in the report. Genre sets the length and
   register (rule S1). Never let a directory's lint template set the length.
2. **Read the whole doc before flagging anything.** Several rules (S3, S4, S7,
   T1, T2) depend on the doc as a whole, not on single sentences.
3. **Build three inventories** and include them in the report:
   - *Fact map*: for each substantive fact, the section that owns it. Flag
     facts that appear in two places, or that another doc owns (S3).
   - *Coinages*: every term the doc introduces that does not appear in the code
     or in an earlier doc, with its use count (T2).
   - *Punchlines*: every verbless closer, "X: Y" apposition, aphorism, and
     "X, not Y" antithesis (T1, T3).
4. **Run the grep pass** below on the file and examine every hit. A hit is a
   candidate, not a finding. Decide each one against its rule.
5. **Run the reading pass** in order: structure and argument (S1–S9), then
   sentences (T4–T16). Check every punchline against the rest of the doc for
   contradiction (T1).
6. **Report** (format below). Do not edit the file unless the user asks you to
   apply the findings.
7. **When asked to apply**, make the edits, then re-run the grep pass and
   report what it still finds.

## Grep pass

Run each command against the doc (`f=path/to/doc.md`). macOS `grep` needs `-E`
for alternation.

```bash
# T3 antithesis density: keep at most one per doc
grep -nE ', not |rather than|instead of' "$f"
# T12 stance hedges and T11 scope defence
grep -nE 'happens to|turns out|notably|worth noting|it is worth|by design|deliberate|intentional|not an oversight' "$f"
# S8 diff-anchored change narration in an evergreen doc
grep -nwE 'now|previously|for now|new|no longer|will be|pilot|currently' "$f"
# T13 label-colon paragraph openers and T14 setup lines
grep -nE '^[A-Z][A-Za-z -]{0,30}: [A-Z]' "$f"
grep -nE '(costs|comes in|has|needs) (us )?(two|three|four|five) (ways|things|pieces|parts)|^[A-Z][a-z]+ pieces:' "$f"
# T10 pseudo-clefts
grep -nE '(^|\. )What [a-z ]+ (is|are) ' "$f"
# T7 copular framings and nominalised subjects (read each hit)
grep -nE '\b(is|are) the (unifying|key|core|central|main|only)\b' "$f"
grep -nE '\b[A-Z]?[a-z]+(tion|ment|ance|ence|ity) (is|are|happens|occurs)\b' "$f"
# T15 nested colon inside parentheses
grep -nE '\([^)]*: [^)]*\)' "$f"
# T16 sembr: lines that end mid-paragraph at a clause boundary
grep -nE '[,;]$' "$f" | head -20
```

## Rule index

Read the reference files for the full rule, its correct and wrong examples,
and its audit method. Use this index to cite rules in the report.

| ID | Rule |
|---|---|
| S1 | Pick length and register from the genre the user named |
| S2 | Write the problem statement free of the solution's vocabulary |
| S3 | Give every fact one home, across sections and across docs |
| S4 | State motivation as what we want, first-person plural |
| S5 | Give one sufficient reason; cut the supporting stack |
| S6 | List only viable alternatives |
| S7 | Make goals checkable against a diff |
| S8 | Write from the merged-world vantage; mark counterfactual hazards |
| S9 | Use bold run-in labels for Design aspects, bullets for true lists, numbered lists for counted enumerations |
| T1 | Ration punchlines to genuine invariants; check each for contradiction |
| T2 | Coin a term only to define it and reuse it |
| T3 | Keep at most one "X, not Y" antithesis per doc |
| T4 | Mark the temporal frame when it shifts between proposed and current behaviour |
| T5 | Order sentences scenario, mechanism, contrast |
| T6 | Put a named agent in charge of each sentence |
| T7 | Replace copulas and nominalisations with verbs |
| T8 | Prefer explicit connectives to semicolon and dash splices |
| T9 | Say the literal thing instead of the metaphor |
| T10 | Un-cleft "What X is Y" unless it stages a real reveal |
| T11 | Replace scope-defence assertions with the reason |
| T12 | Delete stance hedges or state the consequence they gesture at |
| T13 | No label-colon fragments as paragraph openers |
| T14 | Cut setup lines that announce a count or a structure |
| T15 | Never nest a colon inside parentheses inside a colon-list |
| T16 | No semantic line breaks in drafts for sharing |

Two rules invert the godoc conventions in `code-comments`, so never carry
those conventions over:

- In design-doc prose an explicit "so" or "instead" beats a semicolon splice
  (T8). In godoc, illative "so" is a tell.
- Design docs may use a verbless closer for a genuine invariant (T1). Godoc
  never does.

## Report format

Lead with the genre and the three inventories, then the findings grouped by
rule family (S, then T), ordered by how much each finding costs the reader.

For each finding give:

```
L<line> [<rule id>] "<quoted text>"
  why: <one sentence: what the reader pays>
  rewrite: <proposed replacement, or "cut">
```

Example:

```
L42 [T11] "Deliberate scope choice, not an oversight."
  why: asserts intent and withholds the reason; deleting it loses no fact.
  rewrite: the reason the scope stops where it does, or cut.
```

Rules for the report:

- Quote the text exactly so the user can search for it.
- Propose a rewrite for every finding that is not a pure cut. A rewrite must
  keep every fact the original carried, or say which fact it drops and where
  that fact belongs (S3, S8).
- Report a pattern once with all its line numbers when the same rule fires
  many times ("T3 fires at L12, L40, L88, L131: keep L88, which states the
  invariant").
- Do not report a candidate that the rule exempts, such as a dangling verb
  recoverable by ellipsis (T6) or a verbless closer that states an invariant
  (T1).
- End with the two or three changes that would most improve the doc, so the
  user can stop there.
