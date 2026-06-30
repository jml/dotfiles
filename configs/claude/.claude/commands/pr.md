---
description: Draft a PR title/description, review the diff in Emacs, then open the draft PR
argument-hint: "[optional base branch, default 'main']"
---

Open a draft PR on the upstream repo, but let me review the full diff and edit the
title/description in Emacs first. **Aborting the description edit cancels the PR** — nothing
is pushed or created until I approve.

## Steps

1. **Work out the coordinates.**
   - Branch: `git branch --show-current`.
   - Base: `$ARGUMENTS` if given, else `main`.
   - Upstream repo `OWNER/REPO`: derive from `git remote get-url upstream` (fall back to
     `origin` if there is no `upstream`).

2. **Draft the description** following my PR conventions (What / Why / Notes, optional Next;
   prose not bullets for What and Why). Shape it so the first line is the PR title and
   everything after the blank line is the body:
   ```
   <one-line title>

   ## What
   ...
   ## Why
   ...
   ```
   Derive a unique temp path from the repo and branch to avoid collisions with concurrent
   sessions in other repos:
   ```bash
   repo=$(git rev-parse --show-toplevel | xargs basename)
   branch=$(git branch --show-current | tr '/' '-')
   descfile="/tmp/claude-pr-desc-${repo}-${branch}.txt"
   ```
   Write it to `$descfile` with the **Write tool**, then store it as the branch description:
   ```bash
   git config "branch.$(git branch --show-current).description" "$(cat "$descfile")"
   ```

3. **Open the diff for review** (non-blocking):
   ```bash
   emacsclient -n -e '(magit-diff-range "BASE...HEAD")'
   ```
   (substitute the base from step 1 — three dots = the PR diff since the branch diverged.)

4. **Gate on the description edit**, with `run_in_background: true`:
   ```bash
   GIT_EDITOR=emacsclient git -c core.commentChar=';' branch --edit-description
   ```
   I review the diff and edit the title/body in the now-prefilled buffer, then `C-c C-c` to
   approve or `C-c C-k` to cancel. You'll be notified on exit — **do not poll**.

5. **On approval (exit 0):**
   - Push to my fork: `git push -u origin <branch>`.
   - Read the (possibly edited) description back and open the PR:
     ```bash
     desc=$(git config "branch.<branch>.description")
     gh pr create --draft --repo OWNER/REPO --head jml:<branch> \
       --title "$(sed -n 1p <<<"$desc")" --body "$(sed '1,2d' <<<"$desc")"
     ```
   - Report the PR URL.

6. **On cancel (non-zero exit):** do not push, do not create the PR. Say it was cancelled.
   The draft description stays in git config; the next `/pr` overwrites it.

## Notes
- `GIT_EDITOR=emacsclient` must be inline: the harness sets `GIT_EDITOR=true`.
- Push happens only after approval, so cancelling leaves nothing on the remote.
- Always use explicit `--repo` and `--head jml:<branch>` — don't rely on gh's remote guessing,
  which gets confused in worktrees.
