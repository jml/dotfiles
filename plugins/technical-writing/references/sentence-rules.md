# Sentence rules

These rules apply in every genre unless a genre reference says otherwise. Each
rule has a slug for citing in a review. Most have an example, and some have a
test that needs no taste, or exceptions. A rule applied where its exception
holds makes the text worse.

Apply these in a review pass. Knowing them helps while drafting, but drafting
doesn't replace the review (principle 9).

Some examples' fixes name an actor or a reason that the original lacks. In each
case the writer knew it. A reviewer who doesn't know the missing fact writes
`TODO: <what is missing>` in the fix.

## Words

### `filler`

Cut very, really, pretty, much, just, actually, and own where they add nothing.
If you need intensity, choose a stronger word.

- Before: "The tests are actually passing now."
- After: "The tests are passing now."

Keep "own" where it carries ownership: "we wrote our own parser".

### `helper-verb`

**Test:** find be, have, can, will, or do in front of another verb. Delete it if
the claim stays the same.

- Before: "Several users have reported the crash."
- After: "Several users reported the crash."

Keep it where removing it changes the claim. "The migration will be complete"
and "the migration is complete" say different things.

### `plain-word`

Prefer the short common word: use, not utilise. Prefer the reader's vocabulary
to a specialist's. A term from another field ("the dual of the approver", "an
idempotent sink") costs a reader who doesn't share the field, and reads as
showing off to one who does.

- Before: "We need a dual of the approver."
- After: "The approver supplies a sufficient condition for merging. This rule
  supplies a necessary one."

### `same-word`

Use the same word for the same thing every time. When a text rotates synonyms
("the entry set", "the per-entry metadata", "the listing"), the reader wonders
whether they are different things. The same applies to verbs: if one section
says an incident was "caused by" a change, another shouldn't say "triggered by"
unless it means something different.

**Test:** for each important noun and verb, search the text for its synonyms.

### `coinage`

Coin a term only to define it and reuse it. A term that names a concept the text
keeps returning to is terminology. A term used once is a flourish, so replace it
with the plain claim it compresses.

A term that appears only in the writer's earlier prose, and never in the
reader's words, the code, or another document, needs unpacking. That includes
terms from an earlier session or an agent's memory notes.

- Flourish: "a mixed ID economy"
- Plain: "clients could no longer rely on IDs for ordering"
- Session coinage: "watch the soak"
- Plain: "leave it running in production for a week, and check the error rate
  before building on it"

**Test:** list every term the text introduces that the code and other documents
don't use. Flag each one that is used once, or used before it is defined.

### `literal`

Replace a metaphor with the literal claim. Watch for verbs of motion, journeys,
or bodies applied to things that don't move: "leaves the build path", "comes
home", "breathes".

- Before: "One credential leaves the build path."
- After: "The build uses one credential fewer."

Established vocabulary stays: a pull request "lands".

## Sentences

### `verb-driven`

Let a verb carry the sentence. "X is Y" framings, and nouns ending in -tion,
-ment, -ance, or -ity, often hide the action. Rewriting them often surfaces a
missing fact.

**Test:** find the main verb. If it is be, have, do, make, or use, look for a
verb that says more. Scan for -tion and -ment nouns doing a verb's work.

- Before: "The gateway performs validation of the payload."
- After: "The gateway validates the payload."
- Before: "Verification is the unifying idea."
- After: "The builder pins the reference digest, and the checker verifies each
  rebuild against that digest."

Headings, labels, and table cells may stay nominal.

### `name-the-actor`

Put a named actor in charge of each sentence. An abstract noun as the subject
of an action verb hides who does what, and so does a passive.

**Test:** read each sentence's subject, and ask whether it can perform the verb.
Look for a form of "to be" next to a past participle ("is validated", "was
dropped").

- Before: "The token is validated before the request is processed."
- After: "The auth service validates the token, and then the gateway forwards
  the request."
- Before: "Their presence selects the remote source."
- After: "When the variables are set, the build job reads from the remote
  source."

Keep the passive when the actor is unknown (never invent one), when the actor is
irrelevant ("the server was decommissioned in March"), or when naming a person
would turn a systems failure into blame.

### `positive`

Prefer the positive form, and never stack negatives.

**Test:** find not, no, never, and un-. Try the positive form, and keep
whichever is clearer. A sentence with three negatives is almost always unclear.

- Before: "Skipping flagged runs is deliberately not the same as having no prior
  state."
- After: "Skipping flagged runs leaves earlier runs to consider."

Some negatives are clearer: "This isn't ready to ship." Names follow the same
rule: a boolean called `openPROnConflict` reads better than `noPROnConflict`.

### `one-fact`

In reference text (principles, "Terms"), give each sentence one fact. A sentence
that carries five facts through ", so", ", and", a dash, and a semicolon defeats
a reader looking for one of them.

**Test:** count the em-dashes, semicolons, and ", and" or ", so" joints after a
complete clause. More than one makes the sentence a candidate for splitting.

Explanatory prose, such as a design doc's reasoning, may join clauses when the
joint carries the relation (`connective`).

### `connective`

The home of the "so" rule, which inverts between genres (principle 4).

- **In explanatory prose** (design docs, briefings), say how two clauses relate.
  A semicolon or dash between two complete clauses makes the reader guess
  whether the relation is cause, contrast, or sequence.
  - Before: "Workers keep the old client; they keep their local caches."
  - After: "Workers keep the old client, so they keep their local caches."
- **In reference text** (principles, "Terms"), state the result. A
  "so", "hence", or "which means" that introduces a conclusion usually marks a
  derivation aimed at the reviewer of the change, and the derivation belongs in
  the PR description.
  - Before: "Entries come from the cached tree, so file contents are never read."
  - After: "File contents are never read."

### `subject-early`

Put the subject near the start. Clauses stacked before it make the reader hold
them with nothing to attach them to.

- Before: "After three hours of bisecting and a lot of rerunning the flaky
  suite, Priya found the regression."
- After: "Priya found the regression after three hours of bisecting."

Vary sentence openings, too. When every sentence opens the same way, the text
reads as machine-made.

### `modifier`

An opening descriptive phrase must describe the noun right after it.

- Before: "Written in 2019, we can't remove this shim."
- After: "We can't remove this shim, which was written in 2019."

### `garden-path`

If the end of a sentence forces the reader to reinterpret its beginning,
rewrite it.

- Before: "The image built from the stale layers failed the scan."
- After: "The scan failed on an image that had been built from stale layers."

### `pile-up`

Avoid runs of nouns that could each be read as verbs. The reader has to try
every word in every role to find the sentence's shape.

- Before: "Cache store query requests block host updates."
- After: "While the cache store has queries in flight, it blocks host updates."

### `uncleft`

Turn "What X is, is Y" and "What X does is Y" into plain subject, verb, and
object, unless the reveal surprises the reader, or the sentence resolves a
contrast ("what breaks is the index, and the data survives").

- Before: "What each importer keeps after adopting `FetchArchive` is the
  extraction loop."
- After: "After adopting `FetchArchive`, each importer keeps the extraction
  loop."

### `dangling-verb`

Complete a transitive or modal verb whose object the reader would have to
invent. Leave it alone when an earlier verb in the same sentence supplies the
object.

- Complete as written: "`os.Root` guards against symlink tricks that name
  validation cannot." The "cannot" takes "guard against" from earlier in the
  sentence.
- Worse when completed: "…that name validation cannot catch." "Catch" is a
  different verb from "guard against", and adding it narrows the meaning.
- Needs completing: "The wrapper prevents." Prevents what?

### `nested-punctuation`

Never put a colon inside parentheses inside a list introduced by a colon. Three
levels of punctuation in one line make the reader lose track of which level a
clause belongs to. Turn one level into its own sentence.

- Before: "Policy lives in the loop: file modes (`os.Root` is a per-importer
  habit, not a guarantee: it guards against existing symlinks), and
  permission ordering."
- After: "Policy lives in the loop: file modes and permission ordering. `os.Root`
  guards against existing symlinks, but each importer chooses whether to use
  it."

## References and time

### `pronoun`

Each pronoun (it, this, that, they) must have exactly one thing it can refer to.

- Before: "The parser reads the token from the header. If it's malformed, it
  raises."
- After: "The parser reads the token from the header. If the header is
  malformed, the parser raises `ErrMalformedHeader`."

### `long-distance`

Treat every paragraph break, heading, code block, and image as a fresh start.
Name the thing again before using a pronoun for it. When a pronoun's referent is
three paragraphs back, the paragraph in front of the reader can't be read on its
own.

- Before: a paragraph that opens "It also retries on 503."
- After: "The upload client also retries on 503."

### `resolve`

A reference must resolve inside the text, or in something the text links to.
Name the thing.

- Before: "apps are exempt from its rules there"
- After: "apps are exempt from the branch rules that the organisation ruleset
  applies to default branches"

Widely understood concepts can stay unexplained. Things particular to a project
need naming.

### `vantage`

The home of the change-narration rule, which inverts between genres (principle
4).

Text that outlives a change (comments, READMEs, evergreen docs) is read by
someone who never saw the change. Cut words that narrate it: now, no longer,
for now, previously, will be, going forward, and "new", "still", "has been", or
"instead of" when they compare with an earlier version. "Creates a new branch"
and "the job still holds the lock while it retries" are fine.

- Before: "The importer now validates paths instead of trusting the archive."
- After: "The importer validates every path in the archive."

Commit messages and PR descriptions travel with the diff, so describing the
change is their job, and this rule doesn't apply to them.

### `counterfactual`

A hazard that the design or code prevents is true only without it. Never state
it in the indicative next to the thing that prevents it.

- Before: "Apps are exempt from those rules, and a bot pushing from a stale
  clone can rewrite main."
- After: "If these rules lived only in the ruleset, a bot pushing from a stale
  clone could rewrite main."

Or cite the real incident in the past tense: "On 2026-03-02 a bot pushing from a
stale clone rewrote main."

### `frame`

When a proposal under review describes both the proposed system and today's,
mark each switch between them. Use "today" or a date for current behaviour, and
"will" for the proposal where the two are contrasted.

- Before: "The job requires a pinned commit. It silently builds the newer tip."
- After: "The job will require a pinned commit. Today, the job silently builds
  the newer tip."

When the design ships, update the doc to describe the system as it is
(`vantage`). Date the old behaviour ("Until 2026-10, the job built the newer
tip") or cut it.

### `tense-of-proposals`

Say whether a thing has happened, is planned, or is only proposed. "That copy is
deleted" says a deletion happened. If it is a proposal, write "the proposal
deletes that copy".

## Texture

These patterns make text read as machine-written. Readers notice them by their
frequency, so report them with counts. Each rule says where its pattern may
appear.

### `antithesis`

Use at most one "X, not Y" contrast per document, where the contrast is the
point of the sentence. Repeated contrasts make the rhythm itself visible.
Quoted examples don't count.

**Test:** search for ", not ", "rather than", "instead of", and a sentence that
opens "Instead". Keep the one that carries the most weight, and rewrite the rest
as plain claims.

- Before: "Identity, not network position, decides access."
- After: "The caller's identity decides access."

### `punchline`

A punchline is a line built for emphasis. It takes three forms:

- a verbless fragment: "Per-tenant queues instead of a shared one."
- an "X: Y" apposition: "Concurrent workers race harmlessly: lease plus idempotency."
- a short aphorism: "Journal success defines request success."

Use a punchline only to state an invariant, and check each one against the rest
of the text for a contradicting section. Never use one in reference text.

An "X: Y" apposition earns its form when X states the invariant and Y explains it
in a full clause: "A read never waits on a write: readers see the last committed
manifest." When Y compresses mechanisms into a fragment ("lease plus idempotency"), write a sentence instead. A line that is both a punchline and an
"X, not Y" contrast must pass both rules: it states an invariant, and it is the
one contrast the document keeps.

- Keep: "Journal success defines request success." It states an invariant.
- Cut: "Deliberate scope choice, not an oversight." It states nothing about the
  system.
- Overclaim: "Retries are the only way a job re-enters the queue", in a
  document whose later section describes a manual requeue.

### `scope-defence`

"By design", "this is intentional", "deliberately", and "not an oversight"
assert intent and withhold the reason. They answer a reviewer who isn't there.
Give the reason, or delete the sentence if the surrounding text already gives
it.

- Before: "The write path depends on the hosting service. Deliberate scope
  choice, not an oversight."
- After: "The write path depends on the hosting service, because removing that
  dependency needs the new storage tier,
  which a later design covers."

### `stance-hedge`

"Happens to", "turns out", "notably", "it's worth noting", "honestly",
"frankly", and "genuinely" put the writer into a sentence about the system. If
the behaviour is reliable, delete the hedge. If it is incidental and that
matters, state what would break if the behaviour changed.

- Before: "The current importer happens to strip setuid bits."
- After: "The current importer strips setuid bits, but nothing in
  `FetchArchive` requires the next importer to."

### `label-opener`

Don't open a paragraph with a word, a colon, and a fragment. It reads as slide
notes.

- Before: "Main: never. Delegations: none for humans."
- After: "Nobody pushes to main directly. Humans hold no delegations."

A bold run-in label that names one aspect of a design, followed by full
sentences, is fine (`run-in-labels`).

### `setup-line`

Cut a sentence whose only content is that more content follows.

- Before: "That gap costs us in three ways."
- After: the first cost, stated. If the count matters, use a numbered list.

### `decoration`

Watch for more than one em-dash per paragraph, bold on phrases no more important
than their neighbours, lists of three throughout, and a closing sentence that
restates its section. Watch also for a single whimsical phrase in otherwise flat
text, which stands out as decoration.

- Before: "The merged change needs no extra machinery to come home."
- After: "The merged change needs no extra machinery to reach main."

## When not to apply

Applied to text that is already clear, these rules flatten it toward the
average. Don't apply a rule when:

- the fix would state something nobody knows, such as an invented actor
- the fix would strip a hedge that carries evidence
- the original is already clear, and the fix only swaps one acceptable form for
  another
- the fix would put words in the author's mouth that they would never use

---

Rules `filler`, `helper-verb`, `plain-word` (the shorter-word part),
`verb-driven`, `name-the-actor`, `positive`, `subject-early`, `modifier`,
`garden-path`, `pile-up`, `pronoun`, and `long-distance` are adapted from
Michael Lynch's [*Refactoring English*](https://refactoringenglish.com)
(v0.9.8), chapters 5–10. The terms "long-distance pronoun" and "multi-verb
pile-up" are his. The examples are ours.
