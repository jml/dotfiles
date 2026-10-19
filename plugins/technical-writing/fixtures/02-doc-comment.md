# 02: Go doc comment

Genre: doc comment. Reader: a maintainer looking up one fact.

## Text

```go
// Resolve returns the resolved dependency graph for a lockfile from the
// memoised solver pass, so version constraints are evaluated once per lockfile
// and the registry is never contacted — ordering and deduplication happen per
// call, the solve does not. The resolver happens to sort by package name,
// which is by design.
func (r *Resolver) Resolve(ctx context.Context, lf Lockfile) (Graph, error)
```

The type `solveCache` already has a doc comment describing the per-lockfile
cache.

## Must find

- `one-fact`: the first sentence carries several facts.
- `connective` (reference text): "so … the registry is never contacted" puts a
  derivation in a contract sentence. State the guarantee: "It never contacts the
  registry."
- `coinage`: "memoised solver pass" doesn't exist in the code.
- `antithesis` or `punchline`: the closing "…per call, the solve does not".
- Mechanism on the wrong construct: the cache belongs to `solveCache`'s comment.
- `stance-hedge` on "happens to", and `scope-defence` on "by design".

## Must not flag

- Nothing in this comment needs expanding. A fix that adds the derivation back
  as extra sentences is wrong, because the derivation belongs in the PR
  description.

## Source

Repeats casebook cases 1 and 3.
