# Sentence and texture rules (T1–T16)

These rules work at the level of the sentence. T1–T3 need the inventories from
the procedure. T11–T15 are the tells that make a doc read as AI-written, and
most of them can be found with grep. When several fire in one paragraph, the
paragraph usually needs rewriting, not patching.

## T1. Ration punchlines to genuine invariants; check each for contradiction

The verbless closer and the "X: Y" apposition work for genuine invariants.
Used everywhere, the rhythm becomes conspicuous and the load-bearing statements
lose contrast. Aphoristic compression also drifts into overclaim: the memorable
version of a claim is often the one another section contradicts. When precise
and memorable conflict, keep precise.

Keep: "Git success defines RPC success."

Cut: "Deliberate scope choice, not an oversight."

Overclaim: "Escalation is the only pg→git flow." The doc's own merge-replay
section and its Overview both describe another one.

**Audit:** from the punchline inventory, keep only the ones that state an
invariant. Search the rest of the doc for a counterexample to each one kept.

## T2. Coin a term only to define it and reuse it

A coinage that names a concept the doc keeps returning to is terminology. A
coinage used once is a flourish, so replace it with the plain claim it
compresses. A term that appears only in the author's earlier prose, never in
the user's words, the code, or another doc, must be unpacked. A coinage
written into a memory note or an earlier session is still a coinage.

Terminology: *warm*, *follower*, *ownership partition*, each defined and reused.

Flourish: "mixed ID economy", "a kept path to sequence IDs".

Wrong: "watch the soak" ("soak" appeared only in Claude's own earlier text).

Correct: "leave it running in production for a while and check the bots and
error rates".

**Audit:** from the coinage inventory, flag every coinage with one use, and
every coinage used before it is defined.

## T3. Keep at most one "X, not Y" antithesis per doc

The antithesis is the strongest AI-texture tell when it repeats, because the
rhythm itself becomes visible.

Wrong (five in one intent doc): "direction, not design", "a decision, not a
detail", "IAM, not network position", "a second door…, not a new one",
"intent, not design".

Correct: one, where the contrast is the point of the sentence.

**Audit:** the grep-pass hits for `, not |rather than|instead of`. Keep the one
whose contrast carries the most weight, and rewrite the rest as plain claims.

## T4. Mark the temporal frame when it shifts between proposed and current behaviour

A design doc runs two present tenses: design-present for the proposed system
and actual-present for today's behaviour. Never leave adjacent sentences in
different frames unmarked. Mark today's behaviour with "today" or a date, and
use "will" for design-future where the two are contrasted.

Wrong: "The job requires a pinned SHA. It silently builds the newer tip."

Correct: "The job will require a pinned SHA. Today, the ref path silently
builds the newer tip."

**Audit:** in every paragraph that mentions current behaviour, find each switch
between the two systems and check that it is marked.

## T5. Order sentences scenario, mechanism, contrast

State what can happen first, then what detects it and what it does, then how
that differs from today. Stating the rule before the race it guards forces the
reader to hold the rule without knowing why.

Wrong: "A branch that moved since launch fails the execution cheaply."

Correct: "A person can push to the branch after the build launches. The job
compares the branch head with the pinned SHA and fails before building. Today
the ref path has the same race and builds the newer tip."

**Audit:** for each sentence that states a rule or a check, ask whether the
reader already knows the scenario it handles.

## T6. Put a named agent in charge of each sentence

Abstract nouns as subjects of action verbs hide who does what. Name the
component, person, or system that acts.

Wrong: "Their presence selects dataplane sourcing."

Correct: "When they are set, elastic-build selects dataplane sourcing."

Exception: a dangling transitive or modal verb is fine when an earlier verb in
the same sentence supplies its object by ellipsis. "`os.Root` guards against
pre-existing-symlink games that name validation cannot" is complete, and
completing it ("cannot catch") changes the meaning. Fill a dangling verb only
when the reader would otherwise have to invent the object.

**Audit:** read each sentence's subject and ask whether it can perform the
verb.

## T7. Replace copulas and nominalisations with verbs

"X is Y" framings and "-tion/-ment" nouns doing the work of verbs hide the
action. Rewriting them often surfaces a missing fact (who, when).

Wrong: "Verification is the unifying idea." "Source acquisition and
changed-file detection." "Mode selection is per launch."

Correct: "mentat pins the digest, and axlotl verifies each rebuild by diffing
it against that oracle." "Acquiring source code and detecting changed files."
"Mode is selected at launch."

Headers, labels, and doc pointers may stay nominal.

**Audit:** the grep-pass hits for T7, then read each paragraph's first sentence
(copulas cluster there).

## T8. Prefer explicit connectives to semicolon and dash splices

In design-doc prose, an explicit "so", "because", or "instead" tells the reader
how two clauses relate, and a splice leaves them to guess. This inverts the
godoc rule, where illative "so" is a tell. Do not carry godoc rules over.

Wrong: "Agents keep using git; they keep their clones."

Correct: "Agents keep using git, so they keep their clones."

**Audit:** find semicolons and em-dashes joining two complete clauses, and
check whether the relation is causal or contrastive.

## T9. Say the literal thing instead of the metaphor

Wrong: "One credential leaves the build path."

Correct: "One credential is not used in the build path."

Also cut the single whimsical phrase in an otherwise flat doc, such as "needs
no branch machinery to come home". Voice is uniform or absent.

**Audit:** look for verbs of motion, journey, or body ("leaves", "comes home",
"lands", "breathes") applied to non-physical things. "Lands" for a merged PR is
established vocabulary, so leave it.

## T10. Un-cleft "What X is Y" unless it stages a real reveal

The pseudo-cleft puts its payload in end-focus to stage a reveal. Keep it only
when the identity is genuinely surprising, or when it resolves a real contrast
("what breaks is not X but Y").

Wrong: "What each consumer retains after adopting GetTar is the extraction
loop."

Correct: "After adopting GetTar, each consumer retains the extraction loop."

**Audit:** the grep-pass hits for T10.

## T11. Replace scope-defence assertions with the reason

"By design", "this is intentional", "deliberate", and "not an oversight" assert
the author's intent and withhold the reason. They are a review-thread speech
act misfiled into a timeless doc, and inside a Drawbacks section the denial is
redundant with its own location. Give the reason or a pointer to it, and the
deliberateness follows.

Wrong: "Deliberate scope choice, not an oversight."

Correct: the reason itself, or delete the sentence if the surrounding text
already gives it.

**Audit:** the grep-pass hits for T11. Delete the sentence in your head and ask
whether the reader loses a fact about the system.

## T12. Delete stance hedges or state the consequence they gesture at

"Happens to", "turns out", "notably", and "it's worth noting" put the author
into a sentence about the system. If the behaviour is reliable, delete the
hedge. If it is genuinely incidental and that matters, state the risk the
incidence creates.

Wrong: "vmkit's loop happens to hardcode safe modes."

Correct: "vmkit's loop hardcodes safe modes, but nothing in `GetTar` requires
the next consumer to."

**Audit:** the grep-pass hits for T12.

## T13. No label-colon fragments as paragraph openers

A paragraph that opens with a word and a colon followed by a fragment reads as
slide notes. This differs from the S9 bold run-in label, which names a Design
aspect and is followed by full sentences.

Wrong: "Main: never." "Delegations: none for humans." "Threat model."

Correct: "Nobody pushes to main directly." "Humans hold no delegations."

**Audit:** the grep-pass hits for T13, plus any one- or two-word paragraph that
ends in a full stop.

## T14. Cut setup lines that announce a count or a structure

A sentence whose only content is that more content follows makes the reader
wait.

Wrong: "That gap costs us in three ways." "Two pieces of prior art make the
direction cheap." "Three pieces: access, authority, surface."

Correct: state the first cost. If the count matters, make the items a numbered
list (S9).

**Audit:** the grep-pass hits for T14, and any paragraph whose first sentence
contains no fact about the system.

## T15. Never nest a colon inside parentheses inside a colon-list

Two levels of block punctuation collapse into one visual line, and the reader
loses track of which level a clause belongs to. Turn one level into a sentence
or a participle.

Wrong: "(`os.Root` is a per-consumer habit, not a guarantee: it protects
against…)" inside a colon-list.

Correct: "`os.Root` guards against pre-existing symlinks, but it is a
per-consumer habit and `GetTar` does not guarantee it."

**Audit:** the grep-pass hits for T15, and colon-list items that themselves
contain a colon.

## T16. No semantic line breaks in drafts for sharing

jml likes sembr, but nobody else at Chainguard uses it, so drafts for sharing
use ordinary filled paragraphs at about 80 columns. Never introduce sembr unless
asked.

Wrong:

```
The job compares the branch head with the pinned SHA,
and fails before building
when they differ.
```

Correct: the same sentence filled to the paragraph width.

**Audit:** the grep-pass hits for T16. A run of short lines ending at clause
boundaries inside one paragraph is sembr.
