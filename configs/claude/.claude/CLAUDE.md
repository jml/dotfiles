When I ask a question or express uncertainty or curiosity, treat it as genuine. Seize the opportunity to think about the topic afresh.

# Tools
- ALWAYS use `gh` for any interaction with GitHub, including viewing issues. Never use direct HTTP access.

# Writing
@~/src/dotfiles/plugins/technical-writing/references/always-on.md

The technical-writing skills cover each genre. My own conventions on top of them:
- In chat, include the repo in a PR link's text (`[stereo-staging#3757](…)`) when more than one repo is in play. Linear URLs resolve without the slug, so there's no need to look it up just to link a ticket in chat.
- In Markdown prose (design docs, PR descriptions, issue and review comments), cite a Linear ticket as a bracketed reference link: `[DEV-1234]` in the text, with a `[DEV-1234]: https://linear.app/chainguard/issue/DEV-1234/<slug>` definition at the end of the document. Never a bare identifier, never an inline `[DEV-1234](url)` link. Commit messages are plain text, so the bare identifier stays there (`Tracking: DEV-1234`).
- Text posted under my name that I haven't reviewed ends with `_Written by Claude <model name>._`.
- Never use semantic line breaks in text for other people.
- When drafting something I'll send as myself, or when I ask for my voice or style, use the jml-voice skill.

# Code style
- Segregate I/O from pure logic - push side effects to boundaries, keep core functions deterministic and easily testable.
- Use conventional commits
- Stage only the paths that belong to the change; never `git add -A`. If it's unclear what belongs, ask.

## Testing
- Prefer testing on interface boundaries over writing unit tests

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

## Terraform
- Run `terraform init` and `terraform validate` after changing Terraform code
- To apply Terraform changes, run `terraform plan -out tf.out` and present the plan for me to approve. Then run `terraform apply 'tf.out'`
- NEVER run `terraform apply -auto-approve`
