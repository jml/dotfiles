# Casebook

Each case is a piece of agent-drafted text that failed a reader, with the
diagnosis that produced a rule. The rules elsewhere in this plugin state the
verdicts. These cases keep the reasoning, which is what lets a reviewer apply a
rule correctly to a sentence it has never seen, and recognise when its
exception holds.

System names and details are invented. The shape of each failing sentence is
kept from the original.

Each case gives the text and the rules it supports. Most also give what the
reader paid, a diagnosis, the fix, and any limit on the rule.

---

## Doc comments

### 1. The comment that read as machine-written

> `ListEntries` returns the served surface for a revision from the cached tree
> walk, so per-entry metadata is computed once per revision and file contents
> are never read — pagination and prefix filtering happen per request, the walk
> does not.

**What the reader paid.** A maintainer looking for one fact (does this read file
contents?) has to parse five facts joined by "so", a dash, and a comma splice.
"The served surface" and "the cached tree walk" appear nowhere in the code, so
a search for them finds only other comments.

**Diagnosis.** The writer had the whole design in context and was showing
the reviewer that the approach was cheap. That is argument, aimed at the
reviewer of the change, sitting in a sentence whose reader wants the contract.
The closing antithesis ("…per request, the walk does not") is an essayist's
rhythm in reference text.

**Fix.**

> `ListEntries` returns the entries of a revision. It never reads file
> contents. `entryCache` holds the results for each revision.

**Rules.** `one-fact`, `coinage`, `antithesis`, `connective`; principle 4;
code comments: register, and structure (mechanism on the owning construct).

### 2. The coinage that spread

> `// Returns the served surface, filtered by prefix.`

**What the reader paid.** Six comments across a package used "served surface",
and one used "writable surface", for things the code calls `Listing` and
`WritePaths`. Each reader had to work out that these were the same things.

**Diagnosis.** Each new comment matched the most recent comment nearby,
so one session's coinage became the package's vocabulary. A coinage written into
an agent's memory notes spreads the same way, into later sessions.

**Fix.** Use the code's identifiers. Match the plainest neighbour, usually the
interface's doc comment.

**Rules.** `coinage`, `same-word`; code comments: register.

### 3. A coincidence reported as a coincidence

> The importer happens to hardcode safe file modes.

**What the reader paid.** "Happens to" tells the reader that the writer finds
this incidental. It doesn't tell them what follows from that.

**Diagnosis.** The writer stepped into a sentence about the system to
comment on it.

**Fix.** If the behaviour is reliable, state it: "The importer hardcodes safe
file modes." If it is incidental and that matters, state the risk: "The importer
hardcodes safe file modes, but nothing in `FetchArchive` requires the next
importer to."

**Rules.** `stance-hedge`.

### 4. The drumroll opener

> What each importer keeps after adopting `FetchArchive` is the extraction loop,
> and that loop is where policy lives.

**What the reader paid.** The pseudo-cleft holds the payload back to stage a
reveal, and in a comment the reveal is a delay.

**Fix.** "After adopting `FetchArchive`, each importer keeps the extraction loop,
and that loop is where policy lives." The verb "keeps" already carries the
emphasis.

**Limit.** Keep the cleft when the identity surprises the reader, or when the
sentence resolves a contrast ("what breaks is the index, and the data
survives").

**Rules.** `uncleft`.

### 5. The dangling verb that was complete

> `os.Root` guards against symlink tricks that name validation cannot.

The reviewer "completed" this to "…that name validation cannot catch", and the
author changed it back.

**What the reader paid.** Nothing: the sentence was complete. The reviewer's
"fix" would have cost them precision.

**Diagnosis.** "Cannot" takes its verb from "guards against" earlier in
the same sentence, so nothing is missing. The reviewer applied the
dangling-verb rule without checking for that. "Catch" is a different verb, and
adding it quietly narrowed the meaning.

**Fix.** Leave the sentence as the author wrote it.

**Limit.** Complete a dangling verb only when the reader would otherwise have to
invent its object. This rule was written and corrected on the same day. The
correction is the reason the exception exists.

**Rules.** `dangling-verb`.

### 6. Three levels of punctuation in one line

> …sandbox discipline (`os.Root` is a per-importer habit, not a guarantee: it
> protects against existing symlinks that name validation cannot), and the
> fiddly details (permission ordering, mtime application).

**What the reader paid.** A colon inside parentheses inside a colon-led list.
The reader loses track of which level a clause belongs to.

**Diagnosis.** Each aside was added inside the previous one, and nobody
reread the sentence as a whole.

**Fix.** Make one level a sentence, and keep every fact: "…sandbox discipline,
and the fiddly details of permission ordering and mtime application. `os.Root`
guards against existing symlinks that name validation cannot, but each importer
chooses whether to use it."

**Rules.** `nested-punctuation`.

### 7. The hazard stated as if it were happening

> Apps are exempt from those rules there: a bot pushing from a stale clone can
> rewrite main.

**What the reader paid.** The code below the comment prevents this, but the
indicative "can" says it happens. "Those rules there" points outside the
comment.

**Diagnosis.** The writer described the hazard as they had just
discovered it, in the present tense of the session.

**Fix.** "If these rules lived only in the organisation ruleset, a bot pushing
from a stale clone could rewrite main."

**Rules.** `counterfactual`, `resolve`.

---

## Design and intent docs

### 8. "Deliberate scope choice, not an oversight."

From the Drawbacks section of a design doc:

> The write path is unimproved: it still depends on the hosting service for
> latency and availability. Deliberate scope choice, not an oversight.

The author's reaction: "Not only is it missing a verb, it's also… posturing?
redundant? tasteless?"

**Diagnosis.** Each of the three words names a separate failure.

- *Posturing.* It asserts intent and withholds the reason. Deliberateness is
  believable only when the reader can infer it from the reason.
- *Redundant.* It denies an oversight from inside a Drawbacks section. Listing
  something as a drawback already proves it wasn't overlooked.
- *Tasteless.* The verbless form is the strongest emphasis prose has, and here
  it decorates a defensive aside.

Underneath all three, the sentence is a reply to a reviewer, filed in a document
that outlives the review. Deleting it loses no fact about the system.

**Fix.** Give the reason, or a pointer to it: "The write path depends on the
hosting service, because removing that dependency needs the new storage tier,
which a later design covers."

**Rules.** `scope-defence`, `punchline`; principle 6.

### 9. Punchlines everywhere, and one that was false

A design doc closed most paragraphs with a punchline: "Per-tenant queues
instead of a shared one." "Concurrent instances race harmlessly: lease plus
idempotency." "No new deployable." Three of them stated real invariants:

> Journal success defines request success.

One overclaimed:

> Escalation is the only path from the cache back to the origin.

A later section of the same doc described write-back replay, a second path.

**What the reader paid.** At that density the rhythm becomes conspicuous, and
the genuine invariants lose their contrast against the merely stylish ones. The
compressed claim was the one another section contradicted.

**Fix.** Keep the punchlines that state invariants, and write the rest as plain
sentences. Check each one kept against the rest of the doc.

**Rules.** `punchline`; principle 2.

### 10. The flourish and the lone joke

> …which leaves a mixed ID economy.
>
> The merged change needs no branch machinery to come home.

**What the reader paid.** "Mixed ID economy" appears once and compresses a plain
claim: clients could no longer rely on IDs for ordering. "Come home" is the only
whimsy in an otherwise flat document, so it reads as decoration.

**Fix.** "…so clients can no longer rely on IDs for ordering." "The merged
change needs no extra machinery to reach main."

**Limit.** Coinages that the doc defines and reuses ("warm", "lease",
"tenant shard") are terminology, and they stay.

**Rules.** `coinage`, `literal`, `decoration`; design docs: voice.

### 11. The rule stated before the race

> A branch that moved since launch fails the execution cheaply.

The author: "I'm struggling to parse this. Something about the tense maybe?"

**What the reader paid.** "A branch" is the subject of "fails", which is not
something a branch can do. The rule arrives before the race it guards against,
and the doc's other sentences about today's behaviour sit in the same present
tense with no marker.

**Fix.** "Someone can push to the branch after the build starts. The job
compares the branch head with the pinned commit and stops before building. Today
the job has the same race, and builds the newer commit."

**Rules.** `frame`, `name-the-actor`; principle 5; design docs: scenario,
mechanism, contrast.

### 12. Arguing against an idea nobody raised

Design-doc reviews kept finding sentences of the form "The service should
derive this from X rather than Y", where nobody had proposed Y. Sometimes Y had
come up while the doc was being drafted.

**What the reader paid.** Length, and the impression of a debate they missed.

**Fix.** Record an alternative only if a reviewer would plausibly propose it.

**Rules.** `viable-alternatives`; principle 1.

---

## Commits and pull requests

### 13. The description that met the template and failed the reviewer

A one-line fix to a mirror service had a description with all four headings, a
one-sentence What, an unbulleted Why, and a real Next. A reviewer still couldn't
use it:

- The title, What, and the first half of Why described fetch-protocol
  mechanics. The production failure (pulls failing, the mirror rebuilt dozens of
  times a day) appeared mid-paragraph.
- Why asserted the root cause as fact. A note four bullets later admitted that
  the server's behaviour was inferred from reading its source.
- Production counts, cited functions, the alert, and the ticket had no links.
  "This morning" stood in for a date.
- A size estimate rested on an unstated polling interval and was off by about
  a factor of two.
- The real risk was the second of four notes.

**Fix.** Lead with the observable failure, then the diagnosis, then the fix.
Put each hedge in the sentence that makes the claim. Link every source. Derive
the number. Order notes by how much each would change the reviewer's decision.

**Rules.** commits and PRs: lead with what the reviewer must check; principle 2.

### 14. The unedited session output

> Closes the bootstrap gap found by the external review on the design PR.
> Internal adversarial round (security lens, author-as-adversary): no blocking
> findings.

**What the reader paid.** The first sentence assumes the reader saw the design
PR: "the bootstrap gap" and "the external review" are never explained. The one
sentence of context that makes the change make sense arrives at the end of the
paragraph, in a subordinate clause. The self-attestation can't be checked, and
it nudges the reviewer to relax on a PR whose whole subject is that
self-declared authority was the vulnerability.

**Diagnosis.** The description was written from inside the session, where
every reference resolves. The same session's commit message was excellent,
which suggests the problem lay in what was kept from the model's output.

**Fix.** Open with the context a cold reader needs. Replace the self-attestation
with the checkable claim it contained: "With the enforcement reverted, the
end-to-end bootstrap test fails."

**Rules.** principle 1, principle 6; commits and PRs: what belongs in Notes.

### 15. A dense Why that needed a timeline

> With the landing budget bound to the request deadline, one landing can hold
> the slot for its whole deadline. Behind a mutex a queued writer waited that
> long whatever its own deadline, and a request that had already ended then took
> the slot to make a landing nobody was listening to, delaying the writers
> behind it.

The author: "we generated this together, but I can't understand either the PR
description or the decision record."

**What the reader paid.** The full description and decision record used seven
terms (slot, drainer, submitter, change set, pending, taken, withdrawn) without
defining any of them. The rules had no timeline behind them, and each rule
carried its own justification inside the sentence that stated it.

**Diagnosis.** The terms were the session's working vocabulary, and the
writer had the whole sequence of events in mind.

**Fix.** A before-and-after timeline in the PR's Why (`t=0` A takes the slot,
`t=31` B's request ends, `t=60` …), and a decision table in the decision record,
covering event × state → action.

**Rules.** principle 5; `coinage`, `define-early`.

---

## Review comments

### 16. "We don't need X, because Y already does it"

A drafted reply to an over-built PR began by describing what the PR had done
("It adds a separate path with its own CI gate, merge handling, and recovery
label…"). The author wanted it reframed: "I'd rather say something like 'we don't
need to X, Y, Z, since doing A, B, C gets us the same effect but cheaper'. I
also want that thing to be *true*."

The final version, after each claim was checked against the live configuration,
gave one paragraph per unneeded mechanism:

> The bot doesn't need to check CI or review threads, or to merge. The branch
> rules already require passing checks and resolved threads before merging,
> and whoever added the label can press merge.
>
> It also doesn't need to compare label times with branch activity. The hosting
> service already dismisses the bot's approval when a push changes the diff.

**Rules.** review comments: make each point checkable.

### 17. A question turned into an essay

The reviewer's draft comment:

> There are a lot of loops here. What's the algorithmic complexity like?

An agent "improved" it into three bullets surveying each loop, with the finding
(the parent check is quadratic in path depth, 72 seconds at depth 2,000) in the
last bullet. The reviewer found it hard to read: "it buries the finding under a
survey", and it never said "quadratic".

**Fix.** Keep the short question, and post the finding as its own comment on the
offending lines: quadratic in depth, the measurement, how hard it is to trigger,
and the options.

**Rules.** review comments: questions.

### 18. "This" with nothing to point at

> This is the part I think is genuinely new and worth having.

The reviewer: "i honestly have no idea what this is."

**What the reader paid.** An inline comment is read alone. "This" refers to
nothing inside it, and "genuinely" adds stance.

**Fix.** Name the thing: "The local fallback in `changeManifest` exists only in
this copy."

**Rules.** `resolve`, `stance-hedge`.

---

## READMEs

### 19. The README that told its own history

> The unused filesystem module has been retired. There were no production
> importers. Consumers can use the shared walker without an adapter.

The reviewer's whole comment: "README should just say how things are."

Other comments on the same README: "Which claim?", "I don't know what this
sentence means.", "Seems an odd detail to mention.", and "This totally deserves
its own paragraph. Also, why are we being so explicit about the budget?"

**Diagnosis.** The first sentence narrates a change in evergreen text: the
module's retirement is history, and the commit message records it. "Which
claim?" marks a reference that never resolves: the README mentions "the root
claim" without saying what claims it. "I don't know what this sentence means"
fell on sentences that packed several facts each. The budget detail sat in a
paragraph about something else, with no reason given for the reader to care.

**Fix.** Describe the library as it is. Name each claim, and check it. Give each
fact its own sentence, and the budget its own paragraph with its reason, or cut
it.

**Rules.** `vantage`, `resolve`, `one-fact`; READMEs: say how things are;
reference docs: decide what the reader needs.

---

## Explanations and chat

### 20. The status line that hid a false claim

> File a follow-up for the gate to also require prober step outcomes green on
> the new revision, using the histogram and revision metadata from #412.

The reader: "I don't understand what this means."

**Diagnosis.** The sentence packed two ideas together, and #412 didn't
support one of them. The unclear sentence was also wrong.

**Fix.** Split the ideas, and check each claim. When a reader can't follow a
claim, check the claim before polishing the sentence.

**Rules.** principle 1, principle 2; explanations: split compound claims.

### 21. A session coinage presented as shared vocabulary

An agent's status brief turned the word "soak" into a noun ("the soak"), then a
compound ("soak clock"), then a ritual ("watch the soak"). It also told the user
"your comment used 'land and soak'". The user had never typed the word: it first
appeared in the agent's own text, then in a ticket and a PR comment the agent
wrote under the user's account.

The reader: "who talks like that?"

**Diagnosis.** A term that appears only in the agent's own earlier text must
be unpacked ("leave it running in production for a week, and check the error
rate"). And text an agent posted under a person's name is not that person's
wording, so citing it as "you wrote" launders the agent's coinage into their
mouth.

**Rules.** `coinage`; principle 8.

### 22. Undefined terms and a confusing tense

> That is the materialise half of the working copy, so the deploy-bot copy is
> deleted, not migrated.

The reader: "You imply that 'the working copy' has two halves… What's the other?
Also, define each." And: "Your tense here confuses me: has a copy been deleted?"
Nothing had been deleted. "Is deleted" was a proposal's future, badly put.

The reader then asked for something better than piecemeal answers: "let's
pretend I want to talk this through with an intelligent colleague who pretty
much knows what the system is for, but not much else. What's the context she'd
need?"

**Diagnosis.** "Materialise half" used the session's vocabulary. The tense
described a proposal as if it had happened, and "deleted, not migrated" added a
contrast the reader had no use for. The answer came as fragments tied to the
reader's questions, so the reader had to assemble the context.

**Fix.** A short briefing that defines the working copy and its two operations
(writing files out, and collecting changes back), says what each ticket
proposes, and says that nothing has been built or deleted yet.

**Rules.** `tense-of-proposals`, `coinage`, `antithesis`; explanations: pitch it
at an intelligent colleague.

### 23. Vocabulary from another field

An agent proposed describing a new component as "a dual of the approver". The
author: "people don't trust you if you sound too much like a haskell nerd". The
plain version kept the useful idea: the approver supplies a sufficient
condition for merging, and the new rule supplies a necessary one.

**Rules.** `plain-word`; principle 7.

### 24. "Honest" as emphasis

> The honest version of the ticket's Context is: …

The reader: "I like honesty, although am surprised you feel the need to
emphasise it."

On another occasion, about this sentence in a code comment:

> Exhausting the candidates by skipping flagged runs is deliberately NOT the
> honest "no prior state" of the empty case.

The reader: "I can't parse this. I think it's because there are a lot of
negatives. It might also be a conditioned response to 'honest' and
'deliberately'."

**Diagnosis.** "Honest" and "deliberately" are emphasis about the writer's
stance. They tell the reader that the writer takes the claim seriously, and
nothing about the system. Stacked negatives ("NOT", "no prior state") make the
reader track several inversions at once.

**Fix.** Delete the stance words, and state the positive claim: "Skipping
flagged runs leaves earlier runs to consider."

**Rules.** `stance-hedge`, `positive`.
