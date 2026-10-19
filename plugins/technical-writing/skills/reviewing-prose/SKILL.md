---
name: reviewing-prose
description: Reviews the prose of engineering text (design or intent doc, README, PR description, commit message, issue, code comment, review comment, briefing, or message) and reports findings with the quoted text, the rule, what the reader pays, and a proposed fix. Use when asked to review or tighten writing, to "do a prose pass", to check whether text "reads as AI-written" or "slop", to give feedback on the writing in a colleague's doc or PR description, or as the separate review pass after drafting. Gives text an agent drafted an exhaustive pass, and text a person wrote a review that preserves their voice.
---

# Reviewing prose

Review runs as its own pass, separate from drafting, even on text you just wrote
(principle 9 in [principles.md](../../references/principles.md)).

## 1. Choose the posture

- **Agent draft:** text an agent wrote, in this session or another, including
  text the requester says was generated, and text a person lightly edited. The
  goal is to find every failure before a person reads it. Use section 3.
- **Person's writing:** text a person wrote, such as a colleague's design doc or
  the requester's own announcement. The goal is to help the author, and the text
  stays theirs. Use section 4.

If you can't tell which it is, ask.

## 2. Name the genre and the reader

State both at the top of the report. Load
[principles.md](../../references/principles.md),
[sentence-rules.md](../../references/sentence-rules.md), and the genre
reference:

| Genre | Reference |
|---|---|
| Design doc, intent doc, decision record | [design-docs.md](../../references/genres/design-docs.md) |
| README, how-to, reference doc, or instructions for an agent | [readmes-and-howtos.md](../../references/genres/readmes-and-howtos.md) |
| Commit message, PR description | [commits-and-prs.md](../../references/genres/commits-and-prs.md) |
| Issue, ticket, progress comment | [issues.md](../../references/genres/issues.md) |
| Code comment, name, error message | the repository's comment standard if it has one, and [code-comments.md](../../references/genres/code-comments.md) for what it leaves out |
| Review comment or reply, by anyone | [review-comments.md](../../references/genres/review-comments.md) |
| Explanation, briefing, report, message | [explanations.md](../../references/genres/explanations.md) |

Some rules invert between genres (principle 4). Before applying a rule from one
genre to another, check what the second genre's reader is doing.

When you can't decide whether a rule's exception holds, read the casebook
entries that cite the rule in [casebook.md](../../references/casebook.md). They
give the reasoning behind it.

## 3. Agent draft: the full pass

1. **Read the whole text before flagging anything.** Several checks depend on
   the text as a whole.
2. **Run the cold-reader check.** List every term the reader might not know, and
   every reference that might not resolve. For each, say whether the text
   defines it before use, after use, or never.
3. **Check the claims.** For each factual claim, ask what supports it. Where you
   can verify it against code, configuration, or data, do so. Flag claims that
   contradict another part of the text, claims stronger than their evidence, and
   anything that looks invented. When you can't verify a claim, say so in the
   finding, and use `TODO: <what is missing>` in the fix for facts only the
   author has.
4. **For a document,** meaning any design doc, intent doc, README, or reference
   doc (or an excerpt of one), or anything else over about 40 lines, **build
   three inventories:**
   - *Fact map:* each substantive fact, and the section that owns it. Flag facts
     with two homes, and facts another document owns.
   - *Coinages:* each term the text introduces that the code and other documents
     don't use, with its use count.
   - *Punchlines and contrasts:* each `punchline` and each `antithesis`, with its
     line.
5. **Run the grep pass** below. A hit is a candidate. Decide each one against
   its rule and its exceptions.
6. **Read for structure**, using the genre reference, then **for sentences**,
   using the sentence rules.
7. **If the text is a review comment**, check it against review-comments.md as
   well, whoever drafted it.
8. **Report**, using the format below. Edit the text only if asked.
9. **When asked to apply the findings**, make the edits, run the grep pass
   again, and report what it still finds.

### Grep pass

Put the text in a file, set `f` to its path, and run each command. The patterns
read one line at a time, so run the second block as well for hard-wrapped
prose. A command that finds nothing exits with status 1, which is fine.

```bash
# antithesis
grep -nE ', not |rather than|instead of|(^|[.!?] +)Instead\b' "$f"
# scope-defence and stance-hedge
grep -niE 'happens to|turns out|notably|worth noting|it is worth|by design|deliberate|intentional|not an oversight|honest|frankly|genuinely' "$f"
# vantage (expected in commit messages and PR descriptions)
grep -nwiE 'now|previously|for now|new|no longer|will be|still|has been|have been|going forward|anymore|pilot|currently' "$f"
# label-opener (bold run-in labels in design docs are exempt)
grep -nE '^([>*-] +)?"?[A-Z][A-Za-z -]{0,30}: [A-Za-z]' "$f"
# setup-line
grep -niE '\b(two|three|four|five) (ways|things|pieces|parts|reasons|problems)\b' "$f"
# uncleft
grep -nE '(^|[.>"] *|- )What [^.]{1,60} (is|are|does|do) ' "$f"
# verb-driven: copulas and nominalised subjects
grep -nE '\b(is|are) the (unifying|key|core|central|main|only)\b' "$f"
grep -nE '\b[A-Za-z]+(tion|ment|ance|ence|ity) (is|are|happens|occurs)\b' "$f"
# connective: "so" joints (a fix in explanatory prose, a candidate in reference text)
grep -nE '; [a-z]|, so |, hence |which means' "$f"
# filler
grep -nwiE 'very|really|pretty|just|actually' "$f"
# nested-punctuation
grep -nE '\([^)]*: [^)]*\)' "$f"
# decoration: em-dashes, to compare with the number of paragraphs
grep -o '—' "$f" | wc -l
```

```bash
# the same patterns across line breaks: join each paragraph, then search
j=$(mktemp); awk 'BEGIN{RS=""} {gsub(/\n/," "); print}' "$f" > "$j"
grep -noE '.{0,40}(, not |rather than|What [^.]{1,60} (is|are) |(two|three|four|five) (ways|things|pieces|parts)).{0,40}' "$j"
```

The joined search reports paragraph numbers, not line numbers. Find the line in
the original before reporting it.

### Report format

Lead with the genre and the reader, then the cold-reader list, then, for a
document, the three inventories. Then give the findings, grouped as structure,
claims, and sentences, and ordered within each group by how much each costs the
reader.

```
L<line> [<rule>] "<quoted text>"
  why: <one sentence: what the reader pays>
  fix: <proposed replacement, or "cut">
```

Example:

```
L42 [scope-defence] "Deliberate scope choice, not an oversight."
  why: the reader learns the writer feels defensive, and nothing about why the scope stops there.
  fix: the reason the scope stops where it does, or cut.
```

- Count lines from the first line of the text under review. For a file, use the
  file's line numbers. For text inside a fenced block, count from the first line
  inside the fence.
- File each finding under one group: the one where its fix belongs.
- Cite a sentence rule by its slug. Cite a genre rule without a slug as the
  genre and its section, such as `[commits-and-prs: Notes]`, and a principle by
  number.
- Quote exactly, so the text can be searched for.
- Propose a fix for every finding that isn't a cut. A fix keeps every fact the
  original carried, or says which fact it drops and where that fact belongs.
- Report a repeated pattern once, with every line number, and say which
  instance to keep.
- Leave out grep hits that a rule exempts, such as a recoverable dangling verb,
  a punchline that states an invariant, or change narration in a PR
  description.
- End with the two or three changes that would most improve the text, so the
  reader can stop there.

## 4. Person's writing: helping the author

1. **Read it once as the intended reader.** Ask the two questions every reader
   asks: is this relevant to me, and is it worth my time? Note where each answer
   becomes yes. If either never does, that is the headline finding.
2. **Read it again as an editor**, using the genre reference, and the sentence
   rules only where they find a real problem for the reader.
3. **Report in this order:**
   - what works, so the author knows what to protect
   - substance: claims that look wrong, missing context, and design questions,
     phrased as questions for the author
   - structure: where the reader gets lost, and what could be cut
   - sentences: the pattern the author repeats most, with three examples and a
     count
4. **Cap it at about ten findings.** If there are more, say how many, and in
   which categories.
5. **Preserve the voice.** A bold claim, a joke, or an unusual phrase stays
   unless it gets in the reader's way. Before suggesting a hedge, check whether
   it makes the claim more accurate or only more cautious.
6. **When the writing is good, say so and stop.**
7. **When the findings will become comments** on the author's PR or document,
   follow review-comments.md.

## 5. When a review teaches a new rule

If a review, or the requester's reaction to one, reveals a failure that no rule
covers, propose a new rule:

- Add it to the one reference file where it belongs, with a before and an after,
  and any exception. If [always-on.md](../../references/always-on.md) summarises
  the rule's area, update its bullet too.
- Add a casebook entry in the casebook's format, with system names replaced.
- Add a fixture if the failure is easy to reproduce, using a text that differs
  from the casebook entry.

Rules live only in this plugin's reference files, so that there is one version
of each. Never record one in an agent's memory notes or a separate style file.
