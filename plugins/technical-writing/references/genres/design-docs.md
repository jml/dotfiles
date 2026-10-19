# Design docs, intent docs, and decision records

**Reader:** reviewers deciding whether the design is right, implementers
(including agents) building against it, and later readers asking why the system
is the way it is. Reviewers are busy, and the less effort comprehension costs
them, the more attention they have for the design itself.

Use the repository's template and linter when they exist. A template's required
headings set the structure, and the genre sets the length (`genre-length`).

## The three genres

- **Intent doc.** States the problem and the outcome we want, so that people can
  agree on both before anyone designs a mechanism. It covers the problem, the
  proposed outcome, success criteria, scope, constraints, assumptions, and open
  questions, in a page or two (`genre-length`). It stops at the direction, and
  leaves the mechanism to the design.
- **Design doc** (including an RFC). Carries the decisions: goals and non-goals,
  the design, drawbacks, security impact, and the alternatives that lost. It is
  evergreen: when the design changes, the doc changes first, in a reviewed PR.
- **Decision record** (including an ADR). A log entry explaining a change to a
  design. Write one only when a later reader would ask "why isn't it the obvious
  way?", or when the decision took a lot of discussion. Shape it as before,
  after, and why, with a link to the doc PR. Everything else goes in the PR
  description and the commit message.

## Structure and argument

### `genre-length`

The home of the length rule. Pick the length and register from the genre the
requester named, and never from a template's required headings. An intent doc is
a page or two, up to about 200 lines including lists. A design doc usually fits
in under 250 lines. A directory's lint template doesn't justify filling every
required section to the depth of a full design: keep each section to the
paragraph or short list it needs.

**Audit:** compare the line count with the genre. For an intent doc over about
200 lines, or a design doc over about 250, name the sections that carry the
excess.

### `problem-first`

Background states the problem, how it arose, and why it is worth solving, and
never names the proposed mechanism (principle 5).

- Before: "Nothing bounds the run with a budget, and salvage is missing."
- After: "Nothing ends the run before the platform does, and the platform's
  cancellation throws away work the agent had already finished."

**Audit:** list the design's coined terms and package names, and search
Background for each.

### `one-home`

Give every fact one home (principle 3). When a detail fits both Design and
Security, keep it in Security, where a security reviewer looks for it, and link
to it from Design. Rollout progress, PR numbers, and "done" or "open" status
belong in the tracking issue.

**Audit:** build a map from each substantive fact to the section that owns it,
and flag facts with two homes.

### `we-want`

Lead the motivation with what we want, in the first person plural, before any
outside pressure such as a deadline or another project's dependency. Give
secondary benefits their own paragraph.

- Before: "The new release catalogue turns this integration into a blocker."
- After: "We want deploys to start from the release catalogue, so that they
  don't wait on the CI service. Separately, the change fixes two existing pain
  points:" followed by a numbered list.

### `one-reason`

When one reason decides a question, give that reason. A stack of supporting
reasons invites the reader to argue with the weakest.

**Audit:** find "also", "in addition", and second "because" clauses on one
decision, and ask whether the first reason alone decides it.

### `viable-alternatives`

List the alternatives a competent engineer would propose, in a few lines each,
with the reason each lost. Rejecting an alternative that could never have worked
shows the reader nothing. Leave out ideas that came up only while drafting the
doc, since the reader never heard them (principle 1). The register for rejections
may be candid and informal: "Sounds great, but it isn't implemented yet."

When the repository's convention allows, put each rejected alternative next to
the decision it lost to. An agent implementing the design finds it there.

### `checkable-goals`

A goal must be checkable against a change, so that a reviewer, or an automated
reviewer that checks PRs against the doc, can ask whether a PR advances it.
Number the goals, so that PRs can cite them.

- Before: "Improve performance."
- After: "Reduce p99 latency on the read path below 200 ms."

### `evergreen`

A design doc is read in a world where the design has always existed, so the
`vantage` rule applies. Phrase prevented hazards as counterfactuals
(`counterfactual`). While the doc is a proposal under review, mark each switch
between the proposed system and today's (`frame`). When the design ships,
rewrite those passages to describe the system as it is.

### `run-in-labels`

Write the design as short paragraphs, each opening with a bold label that names
one aspect (`**Pinning**:`) and continuing in full sentences. Use bullets for
true lists: goals, non-goals, drawbacks, alternatives. Use a numbered list when
the text announces a count.

Don't open every paragraph of narrative prose, such as Background, with a bold
lead. That makes the doc read as a list of slides.

### `define-early`

Define every term before the text uses it. If the doc needs many terms, put a
short glossary near the top, ordered so that each definition uses only earlier
ones. Use the project's names for components consistently: "the scheduler" is
ambiguous in a way that a component's own name is not.

## Sentences in design prose

The sentence rules apply. These matter most here:

- **Connectives.** Write the "so" (`connective`, explanatory prose).
- **Scenario, mechanism, contrast.** State what can happen, then what detects it
  and what it does, then how that differs from today (principle 5).
  - Before: "A branch that moved since launch fails the execution cheaply."
  - After: "Someone can push to the branch after the build starts. The job
    compares the branch head with the pinned commit, and stops before building.
    Today the job has the same race, and builds the newer commit."
- **Punchlines for invariants only** (`punchline`).
- **Voice.** Sentences that describe the system carry no whimsy (`decoration`).
  Candour in the judgement sections (non-goals, alternatives, drawbacks) is the
  author's voice, and it stays.

## Concrete forms

The forms in principle 5 apply. In a design doc, a timeline suits anything
involving races or deadlines, and a decision table suits rules that depend on
several conditions, such as event by state.
