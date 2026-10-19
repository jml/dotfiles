---
name: writing-docs
description: Drafts design docs, RFCs, intent docs, decision records (ADRs), READMEs, how-to guides, and reference docs. Use when asked to write one of these, to turn a discussion into a design or intent doc, or to restructure an existing design doc, README, or reference doc for a different reader. Picks the length from the genre, states the problem before the mechanism, drafts finished prose with gaps marked, and hands off to a separate review pass.
---

# Writing docs

## Procedure

1. **Name the genre, the reader, and a length target**, and state them to the
   requester in one line before drafting. If the requester didn't name a genre,
   propose one in the same line. The genres are in
   [design-docs.md](../../references/genres/design-docs.md) (intent doc, design
   doc or RFC, decision record or ADR) and
   [readmes-and-howtos.md](../../references/genres/readmes-and-howtos.md)
   (README, how-to, reference doc). The length comes from the genre, never from a
   template's required headings (`genre-length`).
2. **Find the repository's template, linter, and doc location**, and use them.
   Write the doc where it will live, or where the requester says.
3. **Check that you have the material, and ask for what's missing:**
   - for a design doc: the alternatives a reviewer would propose, and the
     security impact
   - for an intent doc: the problem, the outcome, and the success criteria
   - for a README or how-to: a first command that works
4. **For a design or intent doc, write the problem first**, in words that don't
   mention the solution (`problem-first`). A README or how-to opens with what the
   reader can do.
5. **Draft finished prose**, following
   [principles.md](../../references/principles.md) and the genre reference. Mark
   each fact the conversation didn't establish where it belongs, as
   `TODO: <what is missing>`. Never invent a fact (principle 2).
6. **Label the draft.** For a design or intent doc, say in its header or front
   matter that it is a draft, who drafted it, and whether a person has reviewed
   it. For other genres, say so in the PR description.
7. **Review it in a separate pass**, using the `reviewing-prose` skill in its
   agent-draft posture. Apply the findings that don't need the requester's
   judgement.
8. **Report** the genre, the length, the open TODOs, and the review findings you
   didn't apply.

## When the requester edits the draft

If the requester shares their edited version, compare it with your draft. Each
difference that generalises is a candidate rule. Propose it following section 5
of the `reviewing-prose` skill.
