# Review comments and replies

Two situations share this reference: commenting on someone else's pull request
or document, and replying to review comments on your own. Both apply whether a
person or an agent drafts the comment.

## Commenting on someone else's work

**Reader:** the author, who owns the work and decides what to change. Comments
are read one at a time, often out of order, and sometimes months later through a
link.

### Questions

Ask a question when the author's answer will teach one of you something: "Why?",
"Which claim?", "What does `true` mean here?", or "What problem does this section
solve?" Explaining often shows the author the change they need to make. If you
only want a change, say what change.

When editing a reviewer's draft question, keep it a question, and post any
answer you have as a separate comment.

A short thanks to the author before the substance is fine. Filler praise means a
reflexive compliment opening a reply to a finding, such as "Good catch".

### The author's ownership

- Suggest edits to their document, keeping their structure and wording and
  changing only what needs to change. Rewriting it makes the edit harder to
  accept.
- For an obvious fix, attach a suggestion block, so the author can apply it in
  one click.
- When the author is new to the reviewer, or the review is long, the reviewer
  can offer to talk it through. When drafting for a person, suggest it to them.

### Separate the kinds of problem

- Put a design problem and a prose problem in different comments. Polishing the
  prose of a section whose design is wrong wastes the author's time.
- Give each point its own comment, or its own paragraph in a summary.
- When the approach itself is wrong, say so first, and skip the line-level
  comments.

### Make each point checkable

- When arguing that something is unnecessary, say what already provides it: "The
  bot doesn't need to check CI, because the branch rules already require passing
  checks before merge." Verify that claim before posting it.
- Quote or link the exact text or line you mean. "This paragraph", in an inline
  comment on a long section, is ambiguous.
- Mark how much the point matters: nit, non-blocking, or blocking. A point marked
  "don't block on this" seldom gets acted on unless it has a home, so give a
  deferred point a ticket and an owner.

### Prose problems in someone else's document

Name what the reader experiences, and point to the specific text. "I gave up
reading this section", "I don't know what this sentence means", "'Merge target'
is a new term for me", and "A concrete example would help a lot here" are all
actionable. Explain the grammar only if the author asks. Describe the effect on
a reader who lands on the text a year from now.

## Replying on your own pull request

**Reader:** the reviewer who raised the point, checking whether it was handled.

- Lead with the disposition: fixed, declined, or deferred, with the reason.
  "Fixed in `abc1234`: the dry run now refuses to start without `--releaser`."
- Say which test covers a fix.
- For a declined point, give the reason in a sentence. For a deferred one, link
  the issue that tracks it.
- Skip filler praise such as "Good catch" or "Great point".
- Reply in the thread.

## Text drafted for someone else to post

When an agent drafts a comment or reply that a person will post under their own
name:

- Verify every factual claim, because the person's name carries it.
- Use the person's voice if they have described it, and never attribute the
  agent's words or coinages to them.
- If the person hasn't reviewed the text, end it with a line saying a model
  wrote it, in the form they prefer, such as `_Written by <model>._`
  (principle 8).
- Some messages the person will want to write themselves (explanations.md,
  requests and interpersonal messages).
