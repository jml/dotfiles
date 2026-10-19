---
name: writing-change-records
description: Writes commit messages, pull request descriptions, issues and tickets, progress comments on issues, the text of review comments on someone else's PR or document, and replies to review comments on your own PR. Use when drafting or updating any of these, including a PR description before opening a PR, a bug report, a ticket for another team, or a reply to reviewer or bot feedback. To decide what to say about the prose of someone's document, use reviewing-prose first.
---

# Writing change records

Commit messages, PR descriptions, issues, and review comments all travel with a
change or a piece of work. Their readers have the change or the work in front of
them, so the text must carry what that doesn't show.

## Procedure

1. **Identify the genre, and load its reference:**
   - commit message or PR description:
     [commits-and-prs.md](../../references/genres/commits-and-prs.md)
   - issue, ticket, or progress comment on an issue:
     [issues.md](../../references/genres/issues.md)
   - review comment or reply:
     [review-comments.md](../../references/genres/review-comments.md)
2. **Read the repository's conventions** before drafting: commit format, PR
   template, contributing guide, and `CLAUDE.md` or `AGENTS.md`. Where they
   differ from the reference, they win.
3. **Gather the facts the change doesn't show:** the trigger, the evidence, what
   was tested and what wasn't, and the risks. If a fact you need is missing, ask,
   or write `TODO: <what is missing>`. Never invent a fact (principle 2).
4. **Draft**, following [principles.md](../../references/principles.md) and the
   genre reference.
5. **Check every claim** against the diff, or for an issue, against the
   evidence. Delete any sentence the diff makes redundant, and any claim nothing
   supports.
6. **Review in a separate pass** wherever principle 9 says to, using the
   `reviewing-prose` skill in its agent-draft posture.
7. **Before posting under someone else's name**, show them the text, unless they
   have asked you to post directly. Text they haven't reviewed says a model
   wrote it, in the form they prefer.

When a PR changes during review, update its description (commits-and-prs.md,
"Keep the description true"). When what the team knows about an issue changes,
add a comment saying what changed, and edit the body only if it would now
mislead.
