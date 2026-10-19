---
name: writing-code-comments
description: Writes code comments and doc comments, and the other prose that lives in code: identifiers, error and log messages, bot messages, label descriptions, and test names. Use when adding or revising a comment or docstring, naming a function, type, or flag, writing an error message, or when a review flags a comment as unclear, verbose, or written from the diff's point of view.
---

# Writing code comments

## Procedure

1. **Load [code-comments.md](../../references/genres/code-comments.md)**, and
   the repository's comment standard if it has one, combining them as that
   reference says.
2. **Decide whether the fact belongs in a comment at all**, using the table in
   principle 3 of [principles.md](../../references/principles.md). A fact about
   the change goes in the commit message, and evidence goes in the PR
   description.
3. **Read the neighbouring comments**, starting with the interface or type the
   code implements, and match the plainest of them.
4. **Write the comment** from the vantage of a reader who never saw the diff.
5. **Check it.** Is it true now, and will it stay true after the next change
   here? Does every reference resolve inside it? Does it restate the code?
   Delete what restates the code.
6. **Review in a separate pass** (principle 9), using the `reviewing-prose` skill
   in its agent-draft posture. Review all the comments a change adds in one pass,
   together with its commit message and PR description.
