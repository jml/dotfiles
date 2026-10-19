# 02: Go doc comment

Genre: doc comment. Reader: a maintainer looking up one fact.

## Text

```go
// Token returns a bearer token for repo from the in-memory credential hot
// path, so the token endpoint is called at most once per expiry window and
// credentials are never written to disk — jitter and retry happen per caller,
// the exchange does not. The registry rejects a token five minutes after
// issuing it, so Token replaces any token that is more than four minutes old.
// Tokens happen to be scoped to a single repository, which is intentional.
func (c *Client) Token(ctx context.Context, repo string) (string, error)
```

The type `tokenCache` already has a doc comment describing the per-repository
cache.

## Must find

- `one-fact`: the first sentence carries several facts.
- `connective` (reference text): "so … credentials are never written to disk"
  derives a guarantee from the caching. State the guarantee: "It never writes
  credentials to disk."
- `coinage`: "credential hot path" doesn't exist in the code.
- `antithesis` or `punchline`: the closing "…per caller, the exchange does not".
- Mechanism on the wrong construct: the cache belongs to `tokenCache`'s comment.
- `stance-hedge` on "happen to", and `scope-defence` on "which is intentional".

## Must not flag

- "The registry rejects a token five minutes after issuing it, so Token
  replaces any token that is more than four minutes old." The premise is a
  fact about the registry, which the code can't show. It is the reason for the
  constraint, and it stays in the comment (`connective`).
- Nothing in this comment needs expanding. A fix that adds the derivation back
  as extra sentences is wrong, because the derivation belongs in the PR
  description.

## Source

Repeats casebook cases 1 and 3. The "Must not flag" sentence tests the limit on
`connective` in reference text.
