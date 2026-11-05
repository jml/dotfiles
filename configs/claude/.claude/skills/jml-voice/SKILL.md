---
name: jml-voice
description: Describes how Jonathan Lange (jml) writes, for drafting text he will post or send under his own name, or when he asks for something "in my voice" or "in my style". Covers issues, design docs, PR descriptions, review comments, explanations to him, and Slack messages. Use alongside the technical-writing skills, which still govern content and review.
---

# jml's voice

This describes how jml writes. It doesn't prescribe how anyone else should. It
comes from an analysis of his GitHub issues and design docs in 2025–2026, and
from his review comments.

Text drafted in his voice is still a model's draft. Either he reviews it before
it goes out, or it says a model wrote it (`_Written by Claude <model>._`). Never
attribute a model's coinage or claim to him.

## Throughout

- British spelling ("labelled", "honours", "enrol"), with American "-ize"
  words tolerated. Don't normalise his own text in either direction.
- Short declarative sentences. Flat assertion is the default, with a few hedges
  per document, and those only where the uncertainty is real ("probably",
  "I think").
- No politeness padding. Nudges and requests are direct: "How's this issue
  going?"
- Dry humour, understatement, and allusion in asides and comments, never in the
  sentences that describe the system.
- Emphasis by italicising a single word ("_are_", "_very_"), rarely bold.
- Numbers, money, and percentages whenever they describe the system or measure
  an outcome. A design's motivation rests on a principle, not on counts from a
  recent incident, which go stale and invite argument about the count. He cut
  "merge-bot opened 460 of them…" from both a design doc's Background and its
  PR description.
- Reference-style links.

## Issues

- "We" in bodies ("We want…", "We need…"). "I" in comments.
- The template's sections are filled. Unused sections are deleted, not left as
  placeholders.
- Rationale is short, and says why even for trivial work. Outcome is an
  observable end state.
- Solutions, when present, sit in a separate, tentative section ("Probably
  means: …", "Thinking out loud…").
- Scope fences: "(optional)", "(stretch)", "This ticket excludes…".
- A body ends on something functional: a `See …` line, a scope exclusion, a CC,
  or an invitation ("LMK if I've got this wrong"). Never a sign-off.
- Closing comments give a one-line reason, often pragmatic: "Calling this done.
  There's still some pending work, but that's unlikely to bite us any time
  soon."
- Progress comments run: where it's at, next steps, future ideas.

## Design docs

- He writes a specification to be checked against, not an argument to be read.
  Persuasion is confined to Background and Alternatives, and everything between
  is declarative.
- The Overview is one imperative sentence: "Move X off Y."
- Non-goals follow one shape: a noun phrase, a full stop, a one-sentence reason.
  "Full staging-to-prod pipeline. This tool is internal only and not critical."
- Design is cut by operational concern, not by component.
- Two actors: "we" decides, deploys, and rolls out; "the job" or "the service"
  behaves.
- The register is flat in Design, and candid in Non-goals and Alternatives:
  "Not gonna happen", "Sadly", "colossal pain".
- Correctness is defined by reference: parity with a named artifact, or a
  measured baseline.
- Sentences average 16–20 words.

## Pull request descriptions

- What: one sentence naming the outcome or the shift of responsibility, without
  the mechanism. "Shift responsibility for escalating away from merge-bot and
  instead to the producer bot that knows what matters, using vpa-bot as the
  first producer."
- Why: the principle in a sentence or two, then a candid judgement of the
  status quo. "The bot that owns the workload is the one that best knows what
  to do if it fails. merge-bot raising escalations was only ever a stop-gap."
  For a design-doc PR, the evidence lives in the doc.
- Notes: factual dependencies and side effects, as drafted.
- Once he has reviewed the text, he removes the model-attribution line and the
  generated-with footer.

## Review comments

- Short, genuine questions: "Why?", "Say why.", "Which claim?", "What does `true`
  mean?"
- Reader-effect statements: "I don't know what this sentence means.", "I gave up
  reading this section.", "Seems an odd detail to mention."
- Explicit weight: "Nit:", "Suggestion (non-blocking):".
- Credit for his own confusion where it's his: "My problem, not yours."
- Occasionally an emoji as a tone marker (🤔), never as decoration.
- For a long first review of someone new: "Please feel free to get me on the
  phone… A chunky code review is rarely the best way to meet someone for the
  first time."

## Replies to review on his own PRs

From comparing drafted replies with what he posted on [mono#80410][pr80410]:

- One or two sentences: where it landed, then what changed.
- Conceding: a brief credit ("Good point."), then the change. Don't restate why
  the reviewer is right.
- Holding firm: answer what the concern implies, not the fact it states, with
  the principle in one sentence ("it's still better to have an explicit command
  rather than manual `gcloud`"). Leave the mechanism to the code and docs.
- No commit SHAs. The thread already shows the new commits.

[pr80410]: https://github.com/chainguard-dev/mono/pull/80410

## Explanations written for him

When he asks for an explanation "in my style":

- Sections: Problem, Impact, Solution, then what has changed since and a
  recommendation.
- Open by saying what you checked and what you didn't.
- Plain declaratives, one claim per sentence, bold on at most one sentence.
- End with the decision or action that he needs to take.
