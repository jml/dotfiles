# 05: README section

Genre: README. Reader: an engineer about to use the metrics client.

## Text

```markdown
## Batching

The legacy flush thread has been removed. Nobody depended on it in production.
Callers can use the shared batcher directly now.

`Batcher` enforces the series/label/timestamp ordering contract. `Flush`
independently drains the pending queue and checked label sets, emitting points
to the exporter only after each point's schema and write agree. Unicode label
names are rejected because their normalisation differs between backends. The
batcher allows at most 64 pending flushes and shares a process-wide memory
budget of twice the queue limit, which bounds growth across many batchers even
when each stays under its own limit.
```

## Must find

- `vantage`: the first paragraph is history, and "now" narrates the change.
- `resolve`: "the … ordering contract" is never explained.
- `pile-up` or `verb-driven`: "each point's schema and write agree" uses
  "write" as a noun, with no clear meaning.
- Reference docs, decide what the reader needs: the Unicode sentence is an odd
  detail unless the reader needs it, in which case say why.
- `one-fact`: the budget sentence needs its own paragraph, and needs to say why
  the reader should care.

## Must not flag

- The identifiers `Batcher` and `Flush`. Using the code's own names is correct.

## Source

Repeats casebook case 19.
