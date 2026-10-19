# Code comments, and other text that lives in code

**Reader:** a maintainer who arrives knowing nothing and wants one fact fast.
They will believe what the comment says without checking the code, and an agent
reading the code later has no way to doubt it.

If the repository has its own comment standard (a `code-comments` skill, or a
section in `CONTRIBUTING.md`, `CLAUDE.md`, or `AGENTS.md`), use it, and use this
reference for what it leaves out. Where the two differ, the repository's
standard wins.

## What earns a comment

A comment says something the code can't: a constraint, an invariant, a hazard,
or the reason an obvious-looking alternative would be wrong here. A comment that
restates the code tells the reader nothing, and it turns false when the line
below it changes.

A false comment is a defect. Before writing one, ask whether it is true, and
whether it will still be true after the next change.

Route each fact to its home before styling it (principle 3). The commonest
failure is a comment that explains why the change was made, which belongs in the
commit message.

## The vantage of a comment

A comment is read in a world where the code below it has always existed.

- Cut words that narrate the change (`vantage`).
- Phrase a hazard the code prevents as a counterfactual (`counterfactual`).
- Make every reference resolve inside the comment (`resolve`).
- Never address the reviewer ("as requested", "per review").
- Never copy prose between a comment and the PR description without rewriting
  it. Each is written from its own reader's vantage.

Two cases look like change narration and are constraints:

- **An answer to a reader's question.** If a reader who never saw the review
  would ask the same question ("isn't this a leak?"), the answer belongs in the
  comment. If the question only makes sense against the diff ("why did you move
  this?"), the answer belongs in the PR.
- **A removal condition on something temporary.** When an entry exists only for
  a bounded period, the condition for removing it is a constraint on the code as
  it stands: "Remove this grant when the migration project ends." Rollout
  progress ("piloting in staging", "prod flips next week") stays in the tracking
  issue.

When a comment phrased as history protects a live constraint, rewrite it as the
constraint. "Previously used a mutex; switched to atomic to fix the race"
becomes "A mutex here would reintroduce the race fixed in #123."

## Register

Doc comments are reference text.

- Match the plainest neighbour, usually the interface's doc comment. When a new
  comment matches an elaborate neighbour, it copies that neighbour's coinages.
- Give each sentence one fact (`one-fact`).
- State the guarantee, and leave the derivation to the PR description
  (`connective`). Keep a premise the code can't show, because it is the reason
  a constraint exists: "Callers hold this across a network call, so it has to
  stay lock-free."
- No punchlines and no "X, not Y" contrasts (`punchline`, `antithesis`).
- Use the code's own identifiers. A noun phrase that names something existing
  nowhere in the code ("the served surface") sends the reader on a search that
  finds nothing (`coinage`).
- No stance words such as "deliberately", "happens to", or "by design". Give
  the reason (`scope-defence`, `stance-hedge`).

## Structure

- Put a one-sentence summary first, as its own paragraph, then the body.
- Order the body so that each sentence uses only what earlier sentences
  established: premise, constraint, hazard, design, then operational notes.
  Answer a worry in the sentence that raises it.
- Describe a mechanism on the construct that owns it. A method's comment carries
  only what differs from the interface contract. A cache described on both its
  type and every method that uses it becomes two descriptions that drift apart.

## Known problems

A comment recording a known bug or a risk you are carrying needs a trigger, or
nobody will ever act on it. File an issue, and mark the spot with
`TODO(<issue>)` or `BUG(<issue>)`. Record in the issue why the problem is
tolerable for now.

## Other text that lives in code

**Names.** A reader meets a name without the conversation that produced it.

- Say what the thing does or is. A name more general than its behaviour misleads:
  `GetRevision` for a call that returns one digest suggests it returns the
  whole revision.
- Prefer positive names (`positive`).
- Avoid collisions with existing names. A `sources` package beside a `Source`
  type confuses every reader.
- When a boolean argument's meaning isn't visible at the call site ("What does
  `true` mean?"), replace it with a named type or an enum, such as
  `escalationPolicy`.

**Error messages, log lines, and bot messages.**

- Say what is wrong in plain words, and include the identifiers someone will
  need to debug it.
- Name the cause the user can see. "Approval dismissed after opt-out" leaves the
  person who removed a label guessing. "Approval dismissed because the PR no
  longer carries the label that requested it" tells them what happened.
- Write a message for its own code path. A message reused from another path can
  name a feature the reader isn't using.

**Labels and help text.** Say who applies the label, what it triggers, and the
condition it rests on, so that someone tempted to misuse it can see from the
hover text that it won't work.

**Test names and descriptions.**

- State the behaviour, and avoid "should".
- Avoid empty words: "correctly", "properly", "right".
- Say why the behaviour matters, as well as what it is.
