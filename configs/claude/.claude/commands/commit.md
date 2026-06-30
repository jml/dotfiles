---
description: Draft a commit message, then review & finish it in Emacs/magit
argument-hint: "[optional scope or guidance, e.g. 'wip' or 'just the parser changes']"
---

Commit the current work, but hand the message to me in Emacs for review before it lands.
The blocking editor handoff *is* the review gate — don't try to finalise the commit yourself.

## Steps

1. **Inspect state.** Run `git status` and `git diff --staged`. If nothing is staged, stage
   the changes that belong in this commit — `git add -u` for tracked edits, or specific paths
   matching what we've been working on. Never blindly `git add -A`. If the intended contents
   are ambiguous, ask me before staging.

2. **Draft the message** from the staged diff, following my conventions:
   - Conventional Commits (`type(scope): summary`).
   - Body explains *impact and motivation first*, then summarises the changes.
   - End with the trailer:
     `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`
   - `$ARGUMENTS`, if present, is guidance on scope/intent — fold it in.

   Derive a unique temp path from the repo and branch to avoid collisions with concurrent
   sessions in other repos:
   ```bash
   repo=$(git rev-parse --show-toplevel | xargs basename)
   branch=$(git branch --show-current | tr '/' '-')
   msgfile="/tmp/claude-commit-msg-${repo}-${branch}.txt"
   ```
   Write the draft to `$msgfile` with the **Write tool** (not `echo`/`cat`).

3. **Hand off to Emacs**, with `run_in_background: true`:
   ```bash
   GIT_EDITOR=emacsclient git commit -eF "$msgfile"
   ```
   This opens the message plus the full diff in my running Emacs daemon (git-commit-mode).
   I finish with `C-c C-c` or abort with `C-c C-k`. Because it's backgrounded, you'll be
   notified when I'm done — **do not poll** for completion.

4. **Report the result** once the command exits:
   - Exit 0 → committed. Show `git log -1 --stat`.
   - Non-zero → I aborted. Leave the staging untouched and say so. Don't re-run the commit
     unless I ask.

## Notes
- `GIT_EDITOR=emacsclient` must be inline: the harness sets `GIT_EDITOR=true`, which would
  otherwise make git skip the editor and commit immediately with the raw draft.
- The diff appears below the message because `commit.verbose` is enabled in my gitconfig.
