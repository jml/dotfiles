When I ask a question or express uncertainty or curiosity, treat it as genuine. Seize the opportunity to think about the topic afresh.

# Tools
- ALWAYS use `gh` for any interaction with GitHub, including viewing issues. Never use direct HTTP access.

# Chat output
- Every mention of a GitHub PR, GitHub issue, or Linear ticket in chat is an inline Markdown link to its full URL, e.g. `[#3757](https://github.com/chainguard-dev/stereo-staging/pull/3757)` or `[DEV-1234](https://linear.app/chainguard/issue/DEV-1234)`. This applies to every mention, not just the first, because a bare `#3757` or `DEV-1234` isn't clickable in the terminal. Include the repo in the link text (`[stereo-staging#3757](…)`) when more than one repo is in play. Linear URLs resolve without the slug, so there's no need to look it up just to link a ticket in chat.

# Code style
- Segregate I/O from pure logic - push side effects to boundaries, keep core functions deterministic and easily testable.
- Use conventional commits
- In commit body text, describe the impact and the motivation for the change before summarising the changes themselves.
- Stage only the paths that belong to the change; never `git add -A`. If it's unclear what belongs, ask.

## Comments
- Comments are read in a world where the code below them has always existed; PR descriptions are read at the moment of the change. Write each from its own vantage point, and never reuse prose from one in the other unedited. Test every comment: would it still be true and useful to a reader who never saw the PR?
- Explain intended behaviour, constraints, or motivation for the code as it stands. Motivation for the *change* is not motivation for the code. Do not describe the actual behaviour.
- A hazard the code prevents is only true *without* the code. Never state it in the present indicative next to the code preventing it — phrase it counterfactually, or cite the real incident in the past tense.
  - Bad: "apps are exempt from those rules there: a bot pushing from a stale clone can rewrite main."
  - Good: "if these rules lived only there, a bot pushing from a stale clone could rewrite main."
- Referents must resolve within the comment — name the thing ("the default branches that Standard Branch Protections covers"), don't point at it ("its rules there").
- Route each fact to the channel where it survives:
  - Constraint, invariant, or motivation the code cannot express → comment
  - Why the change was made, what it replaced → commit message
  - Evidence the approach is sound, why this instance was chosen → PR description
  - Rollout state and follow-up plans ("staging pilot", "prod flips later") → tracking issue, with `TODO(<issue>)` in code only if the spot must be findable
- Deleting change-narration from a comment loses nothing: file it in the commit message or PR description instead. Warning signs of a diff-anchored comment: "instead of", "previously", "now", "for now", "new", "pilot", "will be", or pre-empting a question you expect from the reviewer.

## Testing
- Avoid "should" in test descriptions.
- Avoid empty words like "correctly", "properly", "right" in test descriptions.
- Test descriptions or comments should state why we want this behavior, why it is important
- Prefer testing on interface boundaries over writing unit tests

# Design docs & long-form prose
- Ration punchline cadence. The verbless closer and the "X: Y" apposition ("Ownership partition instead of replication.") are effective for genuine invariants ("Git success defines RPC success"); used everywhere, the rhythm becomes conspicuous and the load-bearing statements lose contrast. Keep the few that state invariants; write the rest as plain sentences.
- Coin a term only to define it and reuse it. A coinage that names a concept the doc keeps returning to is terminology; a coinage used once ("mixed ID economy") is a flourish — replace it with the plain claim it compresses.
- Voice is uniform or absent. A single whimsical phrase in an otherwise flat doc reads as decoration.
- Aphorisms overclaim. Check every punchy compression against the rest of the doc — the memorable version of a claim is often the version some other section contradicts. When precise and memorable conflict, keep precise.

# Linear tickets
- In Markdown prose (design docs, PR descriptions, issue and review comments), cite a Linear ticket as a bracketed reference link: `[DEV-1234]` in the text, with a `[DEV-1234]: https://linear.app/chainguard/issue/DEV-1234/<slug>` definition at the end of the document. Never a bare identifier, never an inline `[DEV-1234](url)` link.
- Commit messages are plain text, so the bare identifier stays there (`Tracking: DEV-1234`).

# Workflow

## Git Worktrees

Prefix worktree names with the Linear issue ID when applicable, e.g. `claude -w DEV-123-fix-widget`.

### Forked workflow
All repos use a forked workflow:
- `upstream` = official repo (where PRs get merged)
- `origin` = my fork (where I push branches)

### Pushing and PRs
Push branches to origin: `git push -u origin <branch-name>`, then open a draft PR on upstream with `gh pr create --draft --repo <upstream-org>/<repo> --head jml:<branch>`.

When running other `gh` commands against upstream, pass `--repo <upstream-org>/<repo>` explicitly rather than relying on git config: in worktrees gh can get confused about which remote to target.

### Force-pushing in worktrees
`--force-with-lease` fails with "stale info" in bare-repo worktree setups because worktrees share `refs/remotes/` and the tracking refs get out of sync. Use the explicit form instead:
```bash
sha=$(git ls-remote origin <branch> | cut -f1)
git push --force-with-lease=<branch>:$sha origin <branch>
```

## Pull Requests
- Keep PR descriptions short and clear. Use the headings What, Why, Notes, and optionally Next. Avoid bullet points for What and Why.
- What: state intent in one short sentence. Don't summarise the diff or name files/symbols that are right there in the code.
- Why: explain the trigger and rationale only. Don't extrapolate predicted benefits, narrate counterfactuals ("rather than X"), or restate facts established in linked prior PRs. Match confidence to evidence — prefer "looks correct" over "no false positives" unless you actually measured.
- Notes: must be load-bearing. Ask: would a reviewer make a different decision without this line? If not, cut it. Don't restate numbers, paths, or thresholds visible in the diff. Operational facts that change the risk picture (e.g. "won't page, just warnings") belong here.
- Next: only include this section if there really is follow-up work. Each item is a short bullet point. If there is no follow-up, omit the heading entirely.
- Verify before asserting. Don't claim system state ("running silently in prod", "no regressions seen") unless you've checked.
- Be honest about the strength of evidence. Distinguish between what's established, what's inferred, and what's hypothesised. If the evidence is circumstantial or incomplete, say so and invite discussion.

## Terraform
- Run `terraform init` and `terraform validate` after changing Terraform code
- To apply Terraform changes, run `terraform plan -out tf.out` and present the plan for me to approve. Then run `terraform apply 'tf.out'`
- NEVER run `terraform apply -auto-approve`
