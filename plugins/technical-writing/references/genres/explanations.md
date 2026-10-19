# Explanations, briefings, reports, and messages

This covers text written to inform a particular person or group now: an
explanation in chat, a briefing for someone new to an area, a status update, a
report, an announcement, or a message asking for something. A progress comment
on an issue belongs to [issues.md](issues.md).

**Reader:** a specific person with a specific purpose. Name them before
writing, and ask what they will do with the text.

## Explanations

- **Start from the reader's workflow.** When someone asks how a thing works,
  they usually mean "where does this show up in my day?" Lead with when they
  meet it and what it does for them. "`work` and `review` open new terminal
  tabs" may be the whole answer. Add the mechanism after, and only as much as
  they need.
- **Define each term in the reader's vocabulary.** If the reader already has
  words for the concepts, map your terms onto theirs: "*In your terms:* this is
  the 'move the bot onto the platform' milestone."
- **Pitch it at an intelligent colleague.** Imagine a capable colleague who knows
  what the system is for, and not much else. Give them the context they need, in
  order, as one explanation. A list of piecemeal answers to the reader's
  questions makes them assemble it.
- **Split compound claims.** A sentence that packs two ideas together often
  hides that one of them is unsupported.
- **Draw it** (principle 5). In chat, an ASCII timeline or box diagram works
  where a rendered diagram can't.

## Briefings and reports

- Say who it is for and what it is for in the first lines.
- Lead with the state of play, then the actions in order, each with who does it.
- Say what you checked and what you didn't. Separate what is established from
  what is inferred.
- Group actions by how they get done: writing, quick messages, meeting items,
  and changes to ship.
- Choose a length the reader will read. When an expert reader needs only to
  confirm or correct, pre-fill your guess for each question, so that they only
  have to object where you're wrong.
- Keep causal verbs consistent (`same-word`).
- A to-do list for the reader's own later use goes in execution order, with
  checkboxes, commands, and paths.

## Announcements and status messages

- Say what changes, for whom, and when. Say who is unaffected.
- Check that each claim is accurate for the people who already know the system.
  An announcement that "turns on" something already running confuses them.
- Say how to report a problem, and to whom.
- When promising to check something, say what you will check, concretely, so
  that readers can hold you to it.
- Format for the medium. Slack's proportional font breaks aligned text unless it
  sits in a code block.

## Requests and interpersonal messages

- Lead with the ask.
- Disclose the state of what you're sharing: "I haven't reviewed it thoroughly
  yet, but it's at the point where your feedback would help."
- When drafting for someone else to send, keep it in their voice, and make the
  tone collegial (principle 8). Some messages the person will want to write
  themselves.

## Chat replies

The rules in [always-on.md](../always-on.md) apply to every reply. This is the
home of the link rule: in chat and in Markdown, make every mention of a pull
request, issue, or ticket a link, so that the reader can follow it in one step.
