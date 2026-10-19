# Commit messages and pull request descriptions

**Reader:** a reviewer deciding whether to approve the change, and later
someone running `git log` or `git blame` to learn why the code is the way it is.
Both can see the diff. They can't see why you made the change, what you
rejected, what you couldn't test, or what the change means for them.

The repository's conventions win: commit format, subject length, trailers, PR
templates, and required sections. Some repositories forbid `Co-Authored-By`
trailers, for example. This reference governs the content inside whatever shape
the repository requires.

## The redundancy test

Every line must carry information the diff does not. Machine-written messages
fail by narrating the diff back to the reader. "Updates the handler to also
delete comments. Adds a migration. Adds a foreign key." gives the reviewer three
sentences and no information.

## Commit messages

**Subject.** Describe the effect of the change in the imperative, in the
repository's format (for example `type(scope): summary` for Conventional
Commits). "Stop concurrent sign-ups corrupting the session table" tells the
reader more than "Add a mutex around the connection pool". Most readers see only
the subject.

**Size.** Default to the smallest message that fits:

- a subject alone, when it says everything true and useful
- a subject and a short paragraph, for one to three facts the diff hides
- a longer body, when the change has several audiences or records a decision
  worth preserving

When no line would carry information the diff lacks, a one-line commit message
is the correct message.

**Body.** Lead with the impact and the motivation: what was wrong or missing,
who it affected, and why this change. Then include only lines that pass the
redundancy test:

- alternatives rejected, in a line or two each
- what you couldn't test, and why
- behaviour you had to discover, so the next person doesn't repeat it
- the exact error text someone will search for
- evidence that a test pins the fix ("each new test fails with its fix
  reverted")
- for a change spanning several packages, a short list with one line per
  package saying what changed there, to orient the reader

**Leave out:**

- lists of files touched or functions renamed
- narration of your process ("first I…, then I…")
- the size or difficulty of the change
- questions for the reviewer
- constraints a future maintainer needs, which belong in code comments, ideally
  with a test

Commit messages are plain text, so cite tickets by their bare identifier, in a
trailer if the repository uses one.

## Pull request descriptions

Use the repository's template if it has one. Otherwise use these headings:

- **What:** the intent of the change, in one sentence a reader understands
  without the diff. Name the effect. Leave out files and symbols that the diff
  shows.
- **Why:** the trigger and the rationale, in prose. Describing the old
  behaviour, as a timeline if that helps, is part of the trigger. Don't predict
  benefits, and don't restate what linked earlier PRs established.
- **Notes:** only lines that would change a reviewer's decision. Ask of each
  line whether a reviewer would decide differently without it, and order the
  notes by how much each would change that decision.
- **Next:** only if there is follow-up work, with one short item per line. Omit
  the heading otherwise.

Write What and Why as prose. Use bullets for Notes and Next.

### Lead with what the reviewer must check

Principles 2 and 5 apply. For a PR description they mean:

- For a fix, open with the observable failure. A description that opens with
  protocol mechanics buries the outage.
- Write a bug fix's description as a bug report: the error text verbatim, where
  it happens, how often, and the impact.
- Give each causal link its own sentence, so that the reviewer can doubt each
  one separately.
- Never assert a root cause in Why and then admit in a later note that it was
  inferred. The hedge goes in the sentence that makes the claim.
- Link each source line, query, alert, dashboard, and ticket you cite, and say
  what a ticket established when you first mention it.
- State a regression test's contrast explicitly: it fails without the fix, and
  passes with it.

### Rejected alternatives

Record a rejected alternative only when a reviewer would plausibly propose it.
An alternative raised only while drafting the change is one the reader never
heard (principle 1).

### What belongs in Notes

Keep:

- operational facts that change the risk picture ("this alerts as a warning and
  doesn't page")
- what is untested, and how the reviewer can check it
- evidence that a test detects the bug, such as a mutation check
- environment variables or commands needed to reproduce a failure
- why this instance of the change was chosen, where a reviewer would ask
- unrelated changes mixed into the PR, called out so they can be reviewed

Cut:

- "stacks on #N" once #N has merged
- numbers, paths, and thresholds visible in the diff
- process narration and self-attestation ("an internal adversarial review found
  no blocking issues"), which a reviewer can't verify, and which nudges them to
  relax
- text that restates a code comment

A reason the code itself needs, which a future maintainer would look for, goes
in a code comment or a doc. Reviewers read the description, and later
maintainers mostly read the code and its comments.

### Keep the description true

The description must match the diff. When the PR changes during review, update
the description. A note saying "do not change alert policies", on a PR that now
adds an alert, misleads every later reviewer.

## Replies to review comments on your PR

See [review-comments.md](review-comments.md).
