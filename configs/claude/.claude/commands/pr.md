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

2. **Simplify, if the PR is over 200 lines.** Count the lines changed against the base:
   ```bash
   git diff --numstat BASE...HEAD | awk '{n += $1 + $2} END {print n+0}'
   ```
   At 200 or fewer, go straight to step 3 with gate `small`: a small PR doesn't repay a
   simplify pass. Otherwise the gate is `simplified`: invoke the `simplify` skill with the
   Skill tool and let it apply its fixes. If it leaves the working tree dirty, land those
   changes with the `commit` skill before going on, so the PR diff contains them. If that
   commit is cancelled, stop: don't open the PR.

3. **Draft the description** following my PR conventions (What / Why / Notes, optional Next;
   prose not bullets for What and Why). Shape it so the first line is the PR title and
   everything after the blank line is the body:
   ```
   <one-line title>

   ## What
   ...
   ## Why
   ...
   ```
   Make a fresh temp directory, so that concurrent sessions, even on the same branch, never
   share a draft:
   ```bash
   repo=$(git rev-parse --show-toplevel | xargs basename)
   branch=$(git branch --show-current | tr '/' '-')
   mktemp -d "/tmp/claude-pr-desc-${repo}-${branch}.XXXXXX"
   ```
   Shell variables don't carry over between Bash calls, so use the printed path literally.
   Write the description to `<dir>/PR_DESC` with the **Write tool**, then store it as the
   branch description:
   ```bash
   git config "branch.$(git branch --show-current).description" "$(cat "<dir>/PR_DESC")"
   ```

4. **Open the diff for review** (non-blocking):
   ```bash
   emacsclient -n -e '(magit-diff-range "BASE...HEAD")'
   ```
   (substitute the base from step 1 — three dots = the PR diff since the branch diverged.)

5. **Gate on the description edit**, with `run_in_background: true`:
   ```bash
   GIT_EDITOR=emacsclient git -c core.commentChar=';' branch --edit-description
   ```
   I review the diff and edit the title/body in the now-prefilled buffer, then `C-c C-c` to
   approve or `C-c C-k` to cancel. You'll be notified on exit — **do not poll**.

6. **On approval (exit 0):**
   - Push to my fork: `git push -u origin <branch>`.
   - Read the (possibly edited) description back and open the PR:
     ```bash
     desc=$(git config "branch.<branch>.description")
     CLAUDE_PR_GATE=<gate> gh pr create --draft --repo OWNER/REPO --head jml:<branch> \
       --title "$(sed -n 1p <<<"$desc")" --body "$(sed '1,2d' <<<"$desc")"
     ```
     (`<gate>` is `small` or `simplified`, from step 2.)
   - Report the PR URL.

7. **On cancel (non-zero exit):** do not push, do not create the PR. Say it was cancelled.
   The draft description stays in git config; the next `/pr` overwrites it.

## Notes
- `GIT_EDITOR=emacsclient` must be inline: the harness sets `GIT_EDITOR=true`.
- `CLAUDE_PR_GATE=<gate>` must be inline too: a PreToolUse hook denies any `gh pr create`
  without it, so that PRs can't skip the size check in step 2.
- Push happens only after approval, so cancelling leaves nothing on the remote.
- Always use explicit `--repo` and `--head jml:<branch>` — don't rely on gh's remote guessing,
  which gets confused in worktrees.
