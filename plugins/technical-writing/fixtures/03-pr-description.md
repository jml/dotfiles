# 03: Pull request description

Genre: PR description. Reader: a reviewer with no context on the earlier work.

## Text

```markdown
## What

Fixes the quota leak flagged in the follow-up review (#512), by adding a
`releaseOnCancel` path to `reserveQuota` in `quota.go`.

## Why

The reservation path takes quota before the job starts and releases it when the
job finishes, and a cancelled job never reaches the release, which means quota
drains over time until new jobs can't start, which matters because the nightly
batch depends on it. Agent self-review (correctness and concurrency passes): no
issues found. This should make scheduling far more reliable going forward.

## Notes

- Stacks on #508.
- The cancel timeout is 30 seconds.
- With `releaseOnCancel` removed, `TestCancelledJobReleasesQuota` fails.
```

#508 merged last week. The timeout value is visible in the diff.

## Must find

- What names files and symbols from the diff, and describes the mechanism.
- "the quota leak flagged in the follow-up review" assumes the reader saw that
  review (principle 1).
- Why buries the observable failure (new jobs can't start, and the nightly batch
  depends on it) at the end of a long sentence chain (`one-fact`; principle 5).
- Self-attestation: "Agent self-review … no issues found" (principle 6).
- Predicted benefit with no evidence: "far more reliable going forward".
- Stale note: "Stacks on #508" after #508 merged.
- Diff-visible note: the cancel timeout.

## Must not flag

- "With `releaseOnCancel` removed, `TestCancelledJobReleasesQuota` fails." This
  is the one checkable claim, and it belongs in Notes. A good review says to keep
  it, or to move it up.

## Source

Repeats casebook cases 13 and 14.
