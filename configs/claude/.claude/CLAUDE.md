When I ask a question or express uncertainty or curiosity, treat it as genuine. Seize the opportunity to think about the topic afresh.

# Tools
- ALWAYS use `gh` for any interaction with GitHub, including viewing issues. Never use direct HTTP access.

# Code style
- Segregate I/O from pure logic - push side effects to boundaries, keep core functions deterministic and easily testable.
- Use conventional commits
- In commit body text, describe the impact and the motivation for the change before summarising the changes themselves.

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

# Workflow

## Git Worktrees

Prefix worktree names with the Linear issue ID when applicable, e.g. `claude -w DEV-123-fix-widget`.

### Forked workflow
All repos use a forked workflow:
- `upstream` = official repo (where PRs get merged)
- `origin` = my fork (where I push branches)

### Pushing and PRs
1. Push to origin: `git push -u origin <branch-name>`
2. Create PR using explicit flags to avoid ambiguity:
   ```bash
   gh pr create --draft --repo <upstream-org>/<repo> --head <fork-owner>:<branch>
   ```
   Example: `gh pr create --draft --repo chainguard-dev/mono --head jml:worktree-DEV-123-fix-widget`

The explicit `--repo` and `--head` flags are more reliable than relying on git config, especially in worktrees where gh can get confused about which remote to target.

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
