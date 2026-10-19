# 04: Intent doc excerpt

Genre: intent doc, requested as "a sketch". Reader: the team deciding whether to
pursue the work.

## Text

```markdown
## Problem

This costs us in four ways. Teams file tickets to get a sandbox, which is
toil, not process. Provisioning is a product, not a favour.

Approval: manual. Expiry: none.

## Direction

Three pieces of existing tooling make the direction cheap. Policy, not
people, grants a sandbox. Self-service becomes the front door, not a side
entrance.
```

The full draft is 290 lines, with Alternatives, a threat model, and a See-also
section, because the directory's linter requires those headings.

## Must find

- `genre-length`: a sketch of an intent doc should be a page or two.
- `antithesis`: four "X, not Y" contrasts. Keep at most one.
- `setup-line`: "This costs us in four ways." and "Three pieces of existing
  tooling make the direction cheap."
- `label-opener`: "Approval: manual. Expiry: none."
- `literal`: "the front door, not a side entrance".
- `resolve`: "This" in the first sentence has nothing to refer to.

## Must not flag

- Missing design detail. An intent doc stops at the direction.

## Source

Exercises the `genre-length`, `antithesis`, `setup-line`, and
`label-opener` rules.
