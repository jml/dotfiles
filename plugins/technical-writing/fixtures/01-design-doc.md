# 01: Design doc excerpt

Genre: design doc. Reader: staff engineers reviewing the design.

## Text

```markdown
## Design

The cache owns every artifact from upload to expiry. A read never waits on a
write: readers see the last committed manifest. Uploads race safely: content
addressing plus atomic rename. Eviction is the only way an artifact leaves the
cache.

Clients that listed artifacts by upload time now see them in hash order, which
leaves a scrambled-chronology problem. A pinned artifact can sleep soundly
through any eviction sweep.

## Operations

An operator can remove a corrupted artifact with `cachectl purge <digest>`.

## Drawbacks

- Cold reads are no faster: they still go to the origin bucket. This is
  intentional, and the scope stops here by design.
```

## Must find

- `scope-defence`: "This is intentional, and the scope stops here by design."
- `punchline` overclaim: "Eviction is the only way an artifact leaves the
  cache" contradicts `cachectl purge` under Operations.
- `punchline`: "Uploads race safely: content addressing plus atomic rename."
  should become a plain sentence.
- `coinage`: "scrambled-chronology problem", used once.
- `literal` or `decoration`: "sleep soundly through any eviction sweep".
- `vantage`: "now see them", and "still go" in an evergreen doc.

## Must not flag

- "A read never waits on a write" as a punchline to cut. It states an
  invariant, and nothing in the excerpt contradicts it.

## Source

Repeats casebook cases 8, 9, and 10.
