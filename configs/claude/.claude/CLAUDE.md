When I ask a question or express uncertainty or curiosity, treat it as genuine. Seize the opportunity to think about the topic afresh.

# Tools
- ALWAYS use `gh` for any interaction with GitHub, including viewing issues. Never use direct HTTP access.

# Code style
- Segregate I/O from pure logic - push side effects to boundaries, keep core functions deterministic and easily testable.
- Use comments to explain the intended behaviour of code or the motivation behind the code. Do not describe the actual behaviour.
- Use conventional commits
- In commit body text, describe the impact and the motivation for the change before summarising the changes themselves.

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
- Keep PR descriptions short and clear. Use three separate headings: What, Why, Notes. Avoid bullet points for What and Why. Notes should highlight non-obvious implications, risks, trade-offs, and things reviewers should specifically watch for that aren't apparent from reading the code diff. Avoid stating obvious facts or repeating What/Why.
- Be honest about the strength of evidence. Distinguish between what's established, what's inferred, and what's hypothesised. If the evidence is circumstantial or incomplete, say so and invite discussion.

## Terraform
- Run `terraform init` and `terraform validate` after changing Terraform code
- To apply Terraform changes, run `terraform plan -out tf.out` and present the plan for me to approve. Then run `terraform apply 'tf.out'`
- NEVER run `terraform apply -auto-approve`
