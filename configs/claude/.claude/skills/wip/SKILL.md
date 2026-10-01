---
name: wip
description: Full work-in-progress inventory — open PRs, local worktrees/branches, active Linear issues, Desktop artifacts — synthesized into one report ranked by ease of finishing. Use when the user asks "what's my WIP", "catch me up", "what was I working on", returns after time away, or wants worktrees pruned. Args: "low" ranks for a low-energy day (demotes reading-heavy work); "prune" adds a keep/prune classification of worktrees.
---

# WIP inventory

Goal: one report of everything in flight — GitHub PRs, local worktrees and branches,
Linear issues, Desktop artifacts — ranked by ease of finishing, so the user can pick
work without doing archaeology. Four collectors, then synthesize; don't dump raw
collector output.

## Collector 1 — worktrees and branch↔PR join (prefetched)

!`bash /Users/jml/.claude/skills/wip/scripts/inventory.sh`

If the block above is missing, run the script yourself. Sections:
- `== WORKTREES ==` — every checkout: `root|path|branch|dirty=N|ahead=N|date|subject`.
  `GHOST|` lines are registered worktrees whose directory is gone → `git worktree prune`.
- `== PRS BY BRANCH ==` — every non-main branch, joined to the user's PRs:
  `root|branch|STATE|#num|date`. `NOPR` = the branch exists only locally.
- `== BEHIND MAIN ==` — repos whose main checkout is far behind its remote.

## Collector 2 — PR review detail

Run `bash /Users/jml/.claude/skills/my-prs/scripts/run.sh` for the rich per-PR digest
(review threads, failing checks, blockers). Follow the my-prs skill's categorisation
rules for the PR portion of the report — especially `unresolvedInline` lastAuthor to
decide whose move it is.

## Collector 3 — Linear

`go-linear issue_list --assignee=me` with fields `identifier,title,url,state.name,priority,updatedAt`,
then filter locally: In Progress / In Review are active WIP; Todo is queue; ignore
Done/Canceled/Duplicate/Backlog. (Don't pass `--state` — the name is ambiguous across teams.)
Cross-link issues to branches/PRs when names or topics align; an issue with a matching
dirty worktree is one work item, not two.

## Collector 4 — Desktop artifacts (prefetched)

`== DESKTOP ==` in the Collector 1 output: `date|f-or-d|size|name` for every
top-level ~/Desktop entry, newest first. Dotfiles and `archive/` are excluded —
moving something into ~/Desktop/archive/ is how an artifact is marked delivered
or superseded, so archive contents are by definition not WIP.

Most entries are session outputs — reports, drafts, handoffs — with descriptive
topic-prefixed names. Interpret; don't enumerate:

- Cluster by topic prefix: one cluster = one work item, however many files.
  Versioned siblings (`-v2`, `-2`) mean the older file is superseded.
- Cross-link like Linear: a Linear ID in a name (`dev-2220-…` → DEV-2220), or a
  topic matching a worktree/PR/issue, merges into that work item rather than
  adding a new one.
- The WIP question for an output artifact is "was it delivered?", and the
  filesystem can't answer it. When cross-links don't settle it (linked PR
  merged, issue Done), report the item as a question — "blog-post draft, two
  revisions — published?" — never as a guessed status.
- mtime orders the list but is weak evidence of staleness: an untouched report
  may simply be undelivered. Don't age Desktop items into Parked on mtime alone.
- Screenshots get one aggregate line ("3 screenshots, oldest 24 Jul") unless
  taken today — today's may be live work.
- Report from names, dates, and sizes only. Desktop files can hold secrets, so
  never quote contents unasked; skim a specific file only when the user asks. A
  tiny file whose name suggests a credential (webhook, token, key) is a
  housekeeping anomaly — surface it.

## Synthesis — rank by ease of finishing

Buckets, easiest first. Each entry: one line (linked `repo#N — title` or worktree path),
one sentence naming the specific next action. Never generic "address feedback".

1. **Ready now (minutes)** — approved+green PRs (merge), clean PRs missing only a
   reviewer (name who to ping), one-command actions.
2. **Waiting on others after a nudge** — all feedback addressed, needs re-review.
3. **A focused session each** — mechanical code work: rebases, dep bumps, small dirty
   worktrees, answered-but-unpushed review fixes.
4. **Real work remaining** — unanswered substantive feedback, decisions, unfinished features.
5. **Parked / blocked** — external blockers, archived repos, shelved-with-intent
   (check Linear for planned revivals before calling something dead).
6. **Housekeeping** — stale-worktree count, anomalies. Surface anomalies (dirty main
   checkouts, mass staged deletions, mystery state); never act on them unasked.

Close with "if you only do one thing": the oldest item where silence costs social
capital, or the single quickest unblock.

### Energy modes

Default assumes normal capacity. With `low` in the args: reading-heavy and
judgement-heavy items (design-doc review, long diffs, architectural decisions) move to
Parked regardless of how "ready" they look; lead with mechanical and delegable items,
and offer to do the mechanical ones. Reviewing a doc Claude largely generated counts as
reading-heavy for the author — never present "mark generated doc ready-for-review" as a
zero-effort action.

## Prune mode (`prune` in args)

Classify every non-main worktree:
- **Keep** — open PR, or dirty/committed work newer than ~2 weeks, or tied to an
  active Linear issue (including shelved work an issue plans to revive).
- **Skim** — dirty or local-only work, no open PR, not clearly superseded: summarize
  each diff in one line so the user can say keep/toss without reading it.
- **Prune (safe)** — clean, PR merged or deliberately closed, branch on the remote.
  Remove with `git worktree remove` (no `--force`) + `git branch -D`; keep local-only
  branches (`NOPR`) as refs — they cost nothing and preserve commits.
- **Discard** — old dirty/unpushed leftovers whose PR is long merged/closed. Needs
  `worktree remove --force`. Don't run this directly: mass force-removal is (correctly)
  denied by the permission classifier. Write the reviewed list to a script and have the
  user run it with `! bash <script>`.

Finish with `git worktree prune` per repo to clear GHOST entries.

Desktop items get one extra class: **Archive** — delivered or superseded, per the
user's confirmation in this conversation (never inferred alone). Move to
~/Desktop/archive/ with `mkdir -p` + `mv`; never delete Desktop items. Screenshots
older than ~2 weeks can be offered as a batch.

## Notes

- Worktrees created today that you didn't make are likely other live Claude sessions —
  report them as active, never as stale.
- For a PR untouched for 6+ months, check `gh repo view <slug> --json isArchived`
  before recommending action; archived-repo PRs are inert and belong in Parked.
- In ad-hoc zsh loops, never assign to `path` — zsh ties it to `PATH` and every
  subsequent command silently vanishes. The scripts here are bash for this reason.
- No emojis. Keep entries skimmable; the user is triaging, not reading.

## Extending with more collectors (design each in its own session)

- Running agents: scan `~/.claude/jobs/` for live/recent background sessions.
- OmniFocus: query via `osascript`; needs agreement on which perspectives count as WIP.

## Stable commands to allowlist

- `Bash(bash /Users/jml/.claude/skills/wip/scripts/inventory.sh:*)`
- `Bash(bash /Users/jml/.claude/skills/my-prs/scripts/run.sh:*)` (shared with my-prs)
- `Bash(git worktree prune:*)`, `Bash(gh pr view *)`, `Bash(gh repo view *)`
