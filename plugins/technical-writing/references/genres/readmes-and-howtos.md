# READMEs, how-to guides, and reference docs

These three genres serve different readers, so keep them separate. The
[Diátaxis](https://diataxis.fr) framework draws the same lines, and adds
tutorials.

- A **README** gets a reader from "what is this?" to "I tried it and it works".
- A **how-to guide** helps someone who already knows what they want to do get it
  done.
- A **reference doc** states facts about what is, for a reader looking one up.
  The reasoning goes in a design doc, which the reference links to.

Instructions for agents follow the rules for how-to guides when they give a
procedure, such as a skill, and the rules for reference docs when they list
rules.

## READMEs

**Reader:** someone deciding whether this is for them, and then someone trying
it for the first time.

- **Lead with what the reader can do.** "Review five PRs in parallel without
  losing your place" tells the reader more than "A worktree orchestrator".
- **Put the quick start near the top.** Every paragraph between a reader who has
  decided to try it and their first command delays them.
- **Describe the tool as it is.** When a README lists the first version's
  features first and later features after them, readers have to reconstruct what
  the tool does today. Order the sections by what the reader does.
- **Name sections after the reader's jobs:** "What needs your attention?"
  describes a job, where "Inbox" names a part of the tool.
- **Put reference material and mechanism last**, or in separate files:
  architecture, configuration tables, source layout.
- **Say how things are.** A README is evergreen (`vantage`): "The old module has
  been retired" is history, and the commit message already records it.
- **Treat awkward structure as a signal.** When a feature won't fit under one
  heading, the feature may be doing two jobs. Raise that with the requester or
  the author as a question about the design, and keep the heading as it is.

**Self-check:**

- The first sentence says what the reader can do.
- The first command is visible on the first screen.
- No heading is a bare name of a part of the tool.
- Architecture and full reference are at the bottom, or elsewhere.
- The README describes one tool.

## How-to guides

**Reader:** someone with a specific goal and no patience for background.

- **Open by saying who the guide is for**, as the reader's goals: "Read this page
  if you want to:", then a short list. Say where to go for nearby goals the page
  doesn't cover.
- **Give exact commands**, with one clearly marked value to change.
- **Make it self-contained.** If the reader would have to open another runbook to
  finish the task, bring the steps in, with a link to the source (principle 3),
  or replace them with a script.
- **Say what not to do yet**, where a reader would plausibly try it.
- **Mark what you don't know.** If you don't know how a reader gets access to
  something, write `TODO: <what is missing>` for the author.

## Reference docs

**Reader:** someone who knows what they're looking for.

- State facts about what is. Give rationale a sentence at most, and link to the
  design doc for more.
- Document every field, flag, and option, and say what each one means.
- Keep the reference next to the code it describes, and update it in the same
  change that alters the behaviour.
- Decide what the reader needs, and stop there. An odd detail ("backslashes are
  rejected") either matters to the reader, in which case say why, or it
  doesn't, in which case cut it.
