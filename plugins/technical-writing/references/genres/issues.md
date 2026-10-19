# Issues and tickets

**Reader:** whoever will pick up the work, often on another team and weeks
later, and whoever triages it. They need to know why the work matters and what
done looks like before they need to know how.

Use the tracker's template if the team has one. Delete template sections that
have nothing to say. Mark a missing fact as principle 2 says.

## Every issue

- **The title states the outcome or the defect.** For work: "Get billing data
  into the warehouse". For a bug, one sentence stating the defect and its
  consequence: "Merges onto a stale replica rebuild from scratch, doubling build
  time".
- **Say why**, even for trivial work. One sentence is often enough: "Clusters
  are expensive. They need to scale down when not in use."
- **Write the outcome as an observable end state**, something a person can
  check: "No scaling warnings in the console for the build clusters". A restated
  task ("Clusters are scaled down") gives the reader nothing to check.
- **Fence the scope.** Say what is excluded, and mark optional parts as
  optional.
- **One outcome per issue.** Split related outcomes into separate issues, and
  link them.
- **Link prior and related issues**, and say how each relates.

## Work for another team

When the issue asks another team to do something, state the outcome you want
and why you want it, and leave the design to them. They know their system
better than you do. Put any suggestion in a separate section, marked as
tentative.

## Bugs

Use these sections:

- **Problem:** the observable failure, with measured evidence: dates, counts,
  and links to logs, queries, or code lines. Include the exact error text, so
  that a search finds the issue.
- **Cause:** the mechanism, if known, and how confident you are in it.
- **Why it matters:** the impact, and who bears it.
- **Things to decide:** the options, and the test coverage any fix must carry.
  Leave the choice of fix to whoever takes the issue.
- **Context:** where it was found, and related issues and PRs.

## User-facing changes

When the work changes what someone sees, such as a check's warning or a CLI's
output, include a draft of what they will see. Mark placeholder values as
placeholders.

## Long analysis

Put a long analysis in a document attached to or linked from the issue. Keep
comments for discussion and short updates, because a 400-line comment is hard
to navigate and hard to find again.

## Progress comments

A progress comment on a long-running issue gives the state of play first, then
what's done, what's next, and who does it. A closing comment gives the reason in
one line, including for a "good enough" closure: "The billing alerts handle this
case well enough."
