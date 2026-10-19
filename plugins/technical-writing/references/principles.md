# Principles

These principles explain why the rules in this plugin exist. The genre
references and the sentence rules apply them to particular kinds of text.

The rules target text that an agent drafts. They catalogue the forms a model
produces when nothing steers it. A person's prose fails differently, so the
review skill treats it differently (see the last section).

## How the rules combine

1. A repository's own conventions come first: its commit format, PR template,
   comment standard, and doc templates.
2. The genre reference comes next. Some rules invert between genres (principle
   4), and the genre reference says which way.
3. The sentence rules apply in every genre unless the genre reference says
   otherwise.

The principles decide a case that no rule covers. When following a rule
produces text that fails the principle behind it, the rule needs repair. Report
it as such.

## Terms

- **Cold reader**: someone who reads the text without the context the writer
  had. They didn't see the session or the discussion, and they may not have the
  diff. Most readers of engineering text are cold readers, including the author
  six months later.
- **Genre**: the kind of text, defined by who reads it, when, and what they are
  trying to do. A code comment, a commit message, a design doc, and a Slack
  announcement are different genres.
- **Home**: the one place a fact lives. Every other mention links to it.
- **Reference text**: text a reader consults to look up one fact, such as a doc
  comment, a reference doc, the reference sections of a README, or a list of
  rules. Explanatory text, such as a design doc's reasoning or this file, walks
  the reader through an argument.

## 1. Write for the cold reader

When an agent writes, its context holds the discussion and the change. The
change is the most salient thing it knows, and it can't see the later reader.
Text written from that context reads well to its writer, and often to nobody
else. A person rereading their own text has the same blind spot, because they
still hold the context that makes it make sense.

The symptoms:

- a term used before it is defined, or never defined
- a reference that points outside the text: "its rules there", "the simpler
  version", or a "this" with nothing in the paragraph to refer to
- a term coined earlier in the session, used as if the reader knows it
- narration of the change in text that outlives the change (`vantage`)
- an idea raised during the session, then argued against in a document whose
  reader never heard it

To test for these, ask whether someone who saw none of the session would
understand every term and every reference.

## 2. Make every claim true and checkable

A reader should be able to check the argument as well as follow it.

- Verify system state before asserting it.
- Put each hedge in the sentence that makes the claim, and match its strength
  to the evidence. Say which claims are established, which are inferred, and
  which are guesses.
- Make each claim checkable: link its source, give absolute dates, and derive
  any number. The genre references say what that means for each kind of text.
- Never invent a reason, a constraint, a motivation, an actor, a threat, or a
  target to fill a gap. Mark the gap where the fact belongs, as
  `TODO: <what is missing>`. Readers believe a plausible invention, and they
  notice a gap.
- Check every compressed claim against the rest of the document. The memorable
  version of a claim is often the one another section contradicts. When precise
  and memorable conflict, keep precise.
- Keep the hedges that carry evidence. "This probably fixes the race" makes a
  weaker claim than "this fixes the race", and shortening the first into the
  second changes what the text asserts.
- When a reader can't follow a sentence, check the claim before polishing the
  wording. A sentence is often hard to follow because it is wrong.
- Speculation that someone shared to explain their thinking is either verified
  or left out. It never becomes an assertion.

## 3. Give each fact one home

A fact stated in two places drifts into two different facts. Decide where each
fact lives, state it there, and link to it from everywhere else.

| Fact | Home |
|---|---|
| What the code does | The code |
| A constraint, invariant, or motivation the code can't express, including why an obvious-looking alternative would be wrong here | A code comment |
| Why a change was made, and what it replaced | The commit message |
| Evidence that the approach is sound, and why this instance was chosen | The PR description |
| Rollout progress and follow-up plans | The tracking issue |
| The design as it stands, including the alternatives that lost | The design doc |
| Why a design changed, when a later reader would ask why it isn't the obvious way, or when the change took a lot of discussion | A decision record |

Within a document, a fact belongs to one section. Across documents, a fact that
another document owns gets a link.

One exception: a how-to guide may repeat steps from another document, with a
link to the source, because its reader is in the middle of a task.

## 4. Choose the genre and the reader before writing

The genre sets the length, the register, and the structure. Name the reader and
what they will do with the text. For documents, the genre also sets the length
(`genre-length` in the design-docs reference).

Three rules change between genres, and in each case the change follows from
what the reader is doing:

1. **A "so" that introduces a conclusion.** A reader of reference text wants the
   contract, and a derivation after "so" is aimed at the reviewer of the change.
   A design doc's reader is weighing the reasoning, and a semicolon or dash
   hides how two clauses relate. See `connective`.
2. **Narrating the change.** A comment, a README, or an evergreen doc outlives
   the change, so narrating the change makes it false the day after merge. A
   commit message or PR description travels with the diff, so describing the
   change is its job. See `vantage`.
3. **How often a punchline may appear.** Reference text has no claim that needs
   the emphasis of a punchline, and a design doc has a few. See `punchline`.

When a rule from one genre seems to apply to another, ask again what the second
genre's reader is doing.

## 5. Order the text for the reader

- State the problem before the mechanism, without the solution's vocabulary.
  Someone who has never seen the design should be able to design independently
  from the problem statement.
- For a fix, describe the observable failure, then the diagnosis, then the fix.
- Describe what can happen before the rule that guards against it. A reader
  holding a rule without its reason has nothing to attach it to.
- Put the point before the detail. A reader who stops after the first sentence
  should still have the most important fact.
- When prose has to carry several interacting parts, use a concrete form: a
  worked example with real names and values, a timeline for races and ordering,
  a decision table for rules over several conditions, a diagram for components
  and data flow, or a step-by-step journey of who does what and when.

## 6. Describe the system, and leave out the writer's stance

Some sentences describe the writer's attitude to the system:

- intent: "by design", "deliberately", "this is intentional", "not an oversight"
- emphasis: "honestly", "notably", "it is worth noting", "frankly"
- self-attestation: "thoroughly tested", "an internal review found no blocking
  issues"
- slogans: "One command, one answer:"
- praise: "Good catch!"

To test a sentence, delete it. If the reader loses no fact about the system, it
was stance. Replace it with the reason or the evidence behind the stance. A
reader believes that a choice was deliberate only when they can infer that from
the reason.

## 7. Make the reader do as little work as possible

Each sentence rule removes a piece of work the reader would otherwise do:
finding who acts (`name-the-actor`, `verb-driven`), translating vocabulary they
don't share (`plain-word`), deciding whether two words mean the same thing
(`same-word`), or picking one fact out of several (`one-fact`).

## 8. Keep authorship visible

Text posted under a person's name reads as that person's words. If the person
hasn't reviewed it, it says that a model wrote it. When a model drafts for a
person, nothing the model coined or claimed may be attributed to that person.

When material from a sharp private briefing goes into text another person
reads, keep the diagnosis and make the tone collegial.

## 9. Review in a separate pass

A separate review pass catches many failures that survived drafting with every
rule loaded. A draft that followed the rules has not been reviewed.

Review in a separate pass everything except a short chat reply: any document, PR
description, issue, or code comment, any commit message longer than its subject
line, and anything posted or sent under a person's name. Read the whole text
first, and check the claims as well as the sentences.

## A person's writing

These rules describe how a model fails. Much good human prose uses forms they
flag, and applied to it they flatten it toward the average. To review a
person's writing, use the reviewing-prose skill's posture for a person's
writing, which preserves their voice.

---

Principles 5 and 7, and parts of 1 and 2, draw on Michael Lynch's
[*Refactoring English*](https://refactoringenglish.com). See the README for
attribution.
