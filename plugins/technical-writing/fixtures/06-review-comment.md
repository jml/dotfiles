# 06: Review comment drafted for someone else to post

Genre: review comment on a colleague's PR. Reader: the PR's author.

## Text

```markdown
Thanks for taking this on. This goes well beyond what we discussed. It doesn't
reuse the existing webhook handler. Instead, it adds a second dispatcher that
runs before the handler, with its own retry queue, deduplication table, and
dead-letter topic. This is the bit I think is genuinely valuable, and the
lighter approach misses it.
```

The reviewer wants the comment to say which mechanisms are unnecessary, and
what already provides each.

## Must find

- The comment describes what the PR does, and never says what is unnecessary
  or what already provides it (review-comments: make each point checkable).
- `resolve`: "the bit" points at nothing, and "the lighter approach" is never
  defined.
- `stance-hedge`: "genuinely".
- The replacement claims must be verified before posting.

## Must not flag

- "Thanks for taking this on." A short thanks to a contributor before the
  substance is fine. The rule against filler praise targets a reflexive
  compliment opening a reply to a finding.

## Source

Repeats casebook cases 16 and 18.
