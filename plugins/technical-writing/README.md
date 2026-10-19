# technical-writing

Have your agent write commit messages, PR descriptions, issues, review comments,
code comments, design docs, READMEs, and explanations that someone who wasn't in
the session can follow, and review them before anyone else reads them.

An agent writes with the whole session in its context, so its text reads well to
the agent and often to nobody else. With these skills, an agent writes for the
reader who saw none of the session, and checks every claim. It also reviews its
draft in a separate pass, because that catches many failures that drafting with
the rules in mind misses.

## Install

In Claude Code:

```bash
claude plugin marketplace add jml/dotfiles
claude plugin install technical-writing@jml
```

The skills follow the [Agent Skills](https://agentskills.io) format, so other
agents can use them. Copy the whole `technical-writing` directory into the
agent's skills directory, keeping its layout, because the skills link to
shared files under `references/` by relative path.

To apply a condensed set of the rules to every reply, add
[`references/always-on.md`](references/always-on.md) to your `CLAUDE.md`,
`AGENTS.md`, or system prompt, either pasted in or imported with
`@path/to/always-on.md`.

## Which skill to use

| Skill | Use it for |
|---|---|
| `writing-change-records` | Commit messages, PR descriptions, issues, review comments and replies |
| `writing-code-comments` | Code comments, names, error messages, test names |
| `writing-docs` | Design docs, intent docs, decision records, READMEs, how-tos, reference docs |
| `writing-explanations` | Explanations, briefings, status updates, reports, announcements, messages |
| `reviewing-prose` | Reviewing any of the above, whether an agent drafted it or a person wrote it |

The skills hold procedure. The rules live in `references/`, and each rule has
one home there:

- [`principles.md`](references/principles.md): why the rules exist, and how they
  combine. Start here.
- [`sentence-rules.md`](references/sentence-rules.md): sentence-level rules for
  any genre, each with a slug for citing.
- [`genres/`](references/genres/): what each genre adds.
- [`casebook.md`](references/casebook.md): worked cases of text that failed a
  reader, with the diagnosis behind each rule.
- [`always-on.md`](references/always-on.md): a condensed copy of the most common
  rules for every reply. It records where each of its bullets comes from.

[`fixtures/`](fixtures/) holds texts with known problems, for checking the review
skill after a rule changes.

## Reviewing a person's writing

The rules describe how a model's prose fails. Much good human prose uses forms
they flag, so the review skill treats a person's writing differently: it
preserves the author's voice, says what works, and leaves the text theirs.

A repository's own conventions come first: its commit format, PR template,
comment standard, and doc templates. See principles.md, "How the rules combine".

## Changing the rules

Section 5 of the `reviewing-prose` skill says how to add a rule: give it one
home, a before and an after, a casebook entry, and a fixture if the failure is
easy to reproduce.

## Attribution

Several principles and sentence rules are adapted from Michael Lynch's
[*Refactoring English*](https://refactoringenglish.com) (v0.9.8).
`sentence-rules.md` and `principles.md` each list which. The terms
"long-distance pronoun" and "multi-verb pile-up" are his. The prose and examples
here are ours, and his examples are not reproduced. This is an unofficial
adaptation, and the author has not endorsed it. If you find it useful, buy the
book.
