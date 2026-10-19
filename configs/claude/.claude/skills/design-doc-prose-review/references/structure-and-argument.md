# Structure and argument rules (S1–S9)

These rules work at the level of the document: what it contains, where each
fact lives, and how it argues. Check them before sentence-level rules, since
fixing structure often deletes the sentences that would have drawn texture
findings.

## S1. Pick length and register from the genre the user named

An intent doc says what we want, why, the direction, and the open decisions in
about a page. A design doc carries the decisions, alternatives, and invariants,
and in the developer-platform template typically lands well under 150 lines.
A directory's lint template (required headings, a 500-line cap) does not
license filling every section to design-doc density.

Wrong: asked for "a sketch of an intent doc", producing 313 lines with
Alternatives, a threat model, and a See-also list, because doc-lint required the
headings.

Correct: the same doc at 184 lines, with each required section kept to a
paragraph or a short list.

**Audit:** compare `wc -l` against the genre. For an intent doc over ~120 lines,
or a design doc over ~250, name the sections that carry the excess.

## S2. Write the problem statement free of the solution's vocabulary

Background states the problem, how it arose, and why it is worth solving. It
never names the proposed mechanism. Someone with no knowledge of the design
must be able to design independently from it.

Wrong: "Nothing bounds the run with a budget, and salvage is missing."

Correct: "Nothing ends the run before the platform does, and the platform's
cancellation throws away work the agent had already finished."

**Audit:** list the Design section's coined terms and package names, then grep
Background for each.

## S3. Give every fact one home, across sections and across docs

Before keeping a fact, decide which doc and which section owns it, and link
from everywhere else. Within a doc, a detail that appears in Design and again
in Security Invariants survives only in Security Invariants. Across docs,
delete every fact whose home is another doc.

Wrong: the elastic-build doc restating the dual-write doc's P3-blocker framing,
its grandfathering launch policy, and its materialise-a-temp-ref alternative.

Correct: the elastic-build doc links the dual-write doc once and keeps only
what elastic-build owns.

Also route by channel. Rollout state ("staging pilot", "prod flips later"),
per-PR status, and follow-up plans belong in the tracking issue. Why a change
was made belongs in the commit message. Evidence that an approach is sound
belongs in the PR description.

**Audit:** use the fact map from the procedure. Flag every fact with two homes,
and every PR number, rollout date, or "done/open" status in an evergreen doc.

## S4. State motivation as what we want, first-person plural

Background leads with our own strategic motivation, not only the external
forcing function. Secondary benefits get their own signposted paragraph.

Wrong: "The dual-write design turns this integration into a blocker."

Correct: "We want merge-bot to be able to trigger the build based on the
dataplane, not GitHub. This removes GitHub from the happy path and will make
it possible to shift more dataplane-api operations off GitHub. Separately,
there are two existing pain points. The first is… The second is…"

**Audit:** read the first paragraph of Background and Overview. Flag them if
the only motivation is another project's requirement.

## S5. Give one sufficient reason; cut the supporting stack

When one reason decides a question, the others are padding, and they invite
the reader to argue with the weakest one.

Wrong: dropping the check-run because merge-bot doesn't read it, *and* humans
can't see the branch anyway, *and* escalated branches get real checks.

Correct: "merge-bot doesn't read it; it has its own system for gating merges."

**Audit:** find every sentence or bullet that gives two or more reasons for one
decision ("also", "and besides", "in addition", a second "because"). Ask
whether the first reason alone decides it.

## S6. List only viable alternatives

An alternative that could never have worked is rhetoric: rejecting it shows
nothing. Replace it with the real contender from the design space. The
rejection register may be candid and informal.

Wrong: "Shell out to `git clone --depth=1`", which cannot build a ref-less
branch.

Correct: "CAS / RE-API. Sounds like a great idea, but isn't implemented yet."

**Audit:** for each alternative, ask whether a competent engineer would have
proposed it. Flag the ones whose rejection reason is that they cannot work at
all.

## S7. Make goals checkable against a diff

The doc-driven alignment pass asks whether a PR advances a goal, so a goal must
be decidable. Numbered goals let PRs cite them.

Wrong: "Improve performance."

Correct: "Reduce p99 on the read path below 200 ms."

**Audit:** for each goal, write the check that would decide it. If none exists,
flag the goal and propose a measurable version, or mark it as a gap for the
user.

## S8. Write from the merged-world vantage; mark counterfactual hazards

An evergreen doc is read in a world where the design has always existed. Words
that narrate a change ("now", "previously", "new", "no longer", "for now",
"will be") make it false the day after merge. A hazard the design prevents is
true only without the design, so state it counterfactually, or cite the real
incident in the past tense.

Wrong: "The bot now re-reads the issue. A bot pushing from a stale clone can
rewrite main."

Correct: "The bot re-reads the issue before every publication. Without that
re-read, a bot pushing from a stale clone could rewrite main."

Correct: "On mono#60199 a run reached the cap and was retried from scratch."

**Audit:** the grep-pass hits for S8. Check each hazard sentence near a
mechanism that prevents it for indicative "can" or "will".

## S9. Use bold run-in labels for Design aspects, bullets for true lists, numbered lists for counted enumerations

Design sections are short paragraphs with bold run-in labels (`**Scope**:`,
`**Pinning**:`). Bullets are for true lists: Goals, Drawbacks, Alternatives.
An enumeration announced with a count becomes a numbered list, and two items
that share a description may share a number ("3-4.").

Wrong: "It interacts with GitHub five times: when it clones, when it reads the
PR, when it pushes, and when it comments."

Correct:

```
It interacts with GitHub five times:

1. Clone.
2. Read the pull request.
3-4. Push the branch and update the pull request.
5. Comment.
```

Distinguish the run-in label from the listicle tell (T13). A label names one
aspect of a design and is followed by full sentences about it. A listicle puts a
bold lead on every paragraph of narrative prose, such as Background or
Overview.

**Audit:** look for long bullet walls in Design, and for inline enumerations
announced with a count.
