#!/bin/bash
# WIP inventory collector for the /wip skill.
#
# Walks every git repo under the base directory and emits three pipe-delimited
# sections:
#   == WORKTREES ==      one line per worktree (incl. main checkouts):
#                        root|path|branch|dirty=N|ahead=N|last-commit-date|subject
#                        GHOST| lines mark registered worktrees whose directory
#                        is gone (candidates for `git worktree prune`).
#   == PRS BY BRANCH ==  every non-main branch checked out in a worktree, joined
#                        against the invoking user's PRs (any state) on the
#                        repo's GitHub project: root|branch|STATE|#num|date.
#                        NOPR means the branch exists only locally.
#   == BEHIND MAIN ==    main checkouts more than 50 commits behind their remote,
#                        as a staleness signal for the repo as a whole.
#   == DESKTOP ==        top-level ~/Desktop entries, newest first:
#                        date|f-or-d|size-or-entry-count|name. Dotfiles and
#                        archive/ (the delivered/superseded shelf) are excluded.
#
# The GitHub project is taken from the `upstream` remote when present, else
# `origin` — matching the forked workflow where PRs land on upstream.
set -u
BASE="${1:-$HOME/src/chainguard}"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Bare roots appear in the porcelain output as "worktree <path>\nbare"; printing
# on the HEAD line (absent for bare entries) keeps only real checkouts.
for repo in "$BASE"/*/; do
  git -C "$repo" rev-parse --git-dir >/dev/null 2>&1 || continue
  git -C "$repo" worktree list --porcelain 2>/dev/null |
    awk -v root="${repo%/}" '/^worktree /{wt=$2} /^HEAD /{print root "\t" wt}'
done | sort -u -t$'\t' -k2,2 > "$TMP/worktrees.tsv"

echo "== WORKTREES =="
while IFS=$'\t' read -r root wt; do
  if [ ! -d "$wt" ]; then echo "GHOST|$root|$wt"; continue; fi
  branch=$(git -C "$wt" branch --show-current 2>/dev/null)
  [ -z "$branch" ] && branch="(detached)"
  dirty=$(git -C "$wt" status --porcelain 2>/dev/null | wc -l | tr -d ' ')
  ahead=$(git -C "$wt" rev-list --count @{u}..HEAD 2>/dev/null || echo "-")
  last=$(git -C "$wt" log -1 --format='%cs|%s' 2>/dev/null)
  echo "$root|$wt|$branch|dirty=$dirty|ahead=$ahead|$last"
done < "$TMP/worktrees.tsv"

echo
echo "== PRS BY BRANCH =="
cut -f1 "$TMP/worktrees.tsv" | sort -u | while read -r root; do
  git -C "$root" worktree list --porcelain 2>/dev/null |
    awk '/^branch /{sub("refs/heads/","",$2); if ($2 != "main") print $2}' |
    sort -u > "$TMP/branches.txt"
  [ -s "$TMP/branches.txt" ] || continue
  slug=$( (git -C "$root" remote get-url upstream 2>/dev/null ||
           git -C "$root" remote get-url origin 2>/dev/null) |
          sed -E 's#^(git@github\.com:|https://github\.com/)##; s#\.git$##')
  [ -z "$slug" ] && continue
  if ! gh pr list --repo "$slug" --author "@me" --state all --limit 400 \
       --json number,state,headRefName,updatedAt,mergedAt > "$TMP/prs.json" 2>/dev/null; then
    echo "$root|(gh query failed for $slug)"
    continue
  fi
  # The --author filter goes through the search API, which returns nothing on
  # some repos (e.g. sandbox orgs) even when the PRs exist. The join is by
  # branch name, so on an empty result refetch unfiltered.
  if [ "$(jq length "$TMP/prs.json")" = "0" ]; then
    gh pr list --repo "$slug" --state all --limit 400 \
      --json number,state,headRefName,updatedAt,mergedAt > "$TMP/prs.json" 2>/dev/null
  fi
  while read -r b; do
    jq -r --arg b "$b" --arg root "$root" '
      ([.[] | select(.headRefName == $b)] | sort_by(.updatedAt) | last) as $p |
      if $p == null then "\($root)|\($b)|NOPR||"
      else "\($root)|\($b)|\($p.state)|#\($p.number)|\(($p.mergedAt // $p.updatedAt)[0:10])"
      end' "$TMP/prs.json"
  done < "$TMP/branches.txt"
done

echo
echo "== BEHIND MAIN =="
cut -f1 "$TMP/worktrees.tsv" | sort -u | while read -r root; do
  behind=$(git -C "$root" rev-list --count 'main..main@{u}' 2>/dev/null) || continue
  [ -n "$behind" ] && [ "$behind" -gt 50 ] && echo "$root|behind=$behind"
done

echo
echo "== DESKTOP =="
# Names, dates, and sizes only: Desktop files can hold secrets (tokens,
# webhook URLs), so file contents must never enter the inventory.
find "$HOME/Desktop" -mindepth 1 -maxdepth 1 ! -name '.*' ! -name archive -print 2>/dev/null |
  while IFS= read -r p; do
    mdate=$(stat -f '%Sm' -t '%Y-%m-%d' "$p")
    name=$(basename "$p")
    if [ -d "$p" ]; then
      n=$(find "$p" -mindepth 1 -maxdepth 1 ! -name '.DS_Store' | wc -l | tr -d ' ')
      echo "$mdate|d|$n entries|$name"
    else
      echo "$mdate|f|$(stat -f '%z' "$p")B|$name"
    fi
  done | sort -r
exit 0
