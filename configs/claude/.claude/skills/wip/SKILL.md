---
name: wip
description: Full work-in-progress inventory — open PRs, local worktrees/branches, active Linear issues, Desktop artifacts, recent Claude sessions — synthesized into one report ranked by ease of finishing. Use when the user asks "what's my WIP", "catch me up", "what was I working on", returns after time away, or wants worktrees pruned. Args: "low" ranks for a low-energy day (demotes reading-heavy work); "prune" adds a keep/prune classification of worktrees; "days=N" widens the session window from the default 7.
---

# WIP inventory

Goal: one report of everything in flight — GitHub PRs, local worktrees and branches,
Linear issues, Desktop artifacts, Claude sessions — ranked by ease of finishing, so
the user can pick work without doing archaeology. Five collectors, then synthesize;
don't dump raw collector output.

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
  merged, issue Done, a session's job detail naming the file), report the item
  as a question — "blog-post draft, two revisions — published?" — never as a
  guessed status.
- mtime orders the list but is weak evidence of staleness: an untouched report
  may simply be undelivered. Don't age Desktop items into Parked on mtime alone.
- Screenshots get one aggregate line ("3 screenshots, oldest 24 Jul") unless
  taken today — today's may be live work.
- Report from names, dates, and sizes only. Desktop files can hold secrets, so
  never quote contents unasked; skim a specific file only when the user asks. A
  tiny file whose name suggests a credential (webhook, token, key) is a
  housekeeping anomaly — surface it.

## Collector 5 — Claude sessions (prefetched)

!`bash /Users/jml/.claude/skills/wip/scripts/sessions.sh`

If the block above is missing, run the script yourself. It takes a day count
as its first argument (default 7); rerun it with `days=N` from the args. All
times are UTC. Sections:

- `== LIVE ==` — sessions with a running process: `sid|kind|status|name|cwd|jobId|parkedJobId|source`.
  A `busy` interactive session is the user mid-task in another terminal; an
  `idle` one is a terminal left open on something. Both are active, never stale.
- `== JOBS ==` — background jobs updated in the window, with the user's `intent`
  and the job's own `detail` of its result. `detail` is a claim, not a fact: a
  job that says "PR opened" while the branch shows `NOPR` is a finding, not a
  status. State `blocked` means the job stopped on a permission prompt or a
  question — the user owes it an answer.
- `== SESSIONS ==` — one line per session, newest prompt first: liveness
  (`live-busy`, `live-idle`, `bg-idle`, `job-<state>`, `ended`), first and last
  prompt times, prompt count, transcript size, `card=fresh|stale|none`, title,
  first and last typed prompt.
- `== SESSION PATHS ==` / `== SESSION BRANCHES ==` — every cwd and branch the
  transcript touched. A session that enters a worktree records both the repo
  root and the worktree path.
- `== LINEAGE ==` — `parent|child|reason` (`parked`, `respawned`, `shared-root`).
  One lineage is one work item: report from the newest transcript, date it
  from the oldest.
- `== WORKTREE SESSIONS ==` — `worktree|sid|liveness|last-prompt`, the path
  join against Collector 1's worktrees, precomputed so no path matching is
  needed here.

### State cards

Liveness and job detail settle most sessions. The rest — `ended` or `job-done`
sessions with `card=stale` or `card=none` — need a state card saying where the
conversation stopped. Skip sessions that are plainly not work: a single prompt
with no title, or only slash commands. Each remaining session has a slice of
its transcript tail at `~/.cache/claude-wip/slices/<sid>.txt`: title, first
prompt, cwds, branches, and the last few typed prompts and assistant replies.

Fan out with the Agent tool, `model: "haiku"`, up to five sessions per agent,
all agents launched in one message. Each agent reads only the slice files it
is given and writes `~/.cache/claude-wip/cards/<sid>.json`:

```json
{"sessionId": "<sid>", "transcriptSize": <size= from the slice header>,
 "status": "done | awaiting-user | blocked | mid-task | unclear",
 "doing": "one sentence: what the session was for",
 "stopped_at": "one sentence: the last thing that happened",
 "pending_for_user": "the literal question or decision left open, or null",
 "next_action": "one sentence, concrete",
 "refs": {"prs": ["repo#N"], "linear": ["DEV-N"], "paths": [], "desktop": []},
 "evidence": {"last_prompt": "verbatim", "last_assistant": "verbatim sentence"}}
```

Agent instructions to pass along: status comes only from the slice — an
assistant turn ending in a question or an offer that the user never answered is
`awaiting-user`; a final turn reporting completion is `done`; a final turn
stopping on a permission denial or an external dependency is `blocked`;
anything else is `mid-task`, or `unclear` if the slice doesn't show. Quote
evidence verbatim. Refs come only from text in the slice; never infer them.
Never open the raw transcript.

Trust a card's status only when its evidence supports it. Cards are reused
across runs while the transcript is unchanged (`transcriptSize` matches), so
most runs produce only a handful.

### Merging sessions into work items

- Join by path first (`== WORKTREE SESSIONS ==`), then branch, then Linear IDs
  and PR numbers appearing in titles, prompts, job detail, and card refs.
- Git and GitHub are truth for state (dirty, ahead, PR open/merged); the
  session is truth for intent and stopping point. When they disagree, report
  the disagreement rather than either side.
- A session with no worktree — a brainstorm, a design review, a question asked
  from a main checkout — is still a work item. Cross-link it to Desktop
  artifacts by topic; a job whose detail says it delivered a file to the
  Desktop settles which file it produced, though not whether anyone has read it.
- A worktree with no session in the window is cold regardless of mtime. A
  session whose worktree is gone is finished if its PR merged, otherwise a
  question for the user.
- Slices hold typed prompts and assistant prose only, because tool output is
  where pasted secrets and file contents end up. Never quote tool output and
  never read a raw transcript to fill a gap in a card.

## Synthesis — rank by ease of finishing

Open with one line on what is live right now: busy and idle interactive
sessions by directory, and jobs that are `blocked` or finished-but-unread.

Buckets, easiest first. Each entry: one line (linked `repo#N — title` or worktree path),
one sentence naming the specific next action. Never generic "address feedback".

1. **Ready now (minutes)** — approved+green PRs (merge), clean PRs missing only a
   reviewer (name who to ping), one-command actions, a session or job waiting on
   an answer from the user (quote the question), a finished parked job whose
   result the user hasn't read.
2. **Waiting on others after a nudge** — all feedback addressed, needs re-review.
3. **A focused session each** — mechanical code work: rebases, dep bumps, small dirty
   worktrees, answered-but-unpushed review fixes, a `mid-task` session with a
   clear next action.
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
  active Linear issue (including shelved work an issue plans to revive), or
  touched by a live session.
- **Skim** — dirty or local-only work, no open PR, not clearly superseded: summarize
  each diff in one line so the user can say keep/toss without reading it. A state
  card for the worktree's last session often answers this without reading the diff.
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

- A worktree whose session is `live-busy` belongs to another Claude session that
  is working right now: report it as active, never prune it, never edit in it.
- For a PR untouched for 6+ months, check `gh repo view <slug> --json isArchived`
  before recommending action; archived-repo PRs are inert and belong in Parked.
- In ad-hoc zsh loops, never assign to `path` — zsh ties it to `PATH` and every
  subsequent command silently vanishes. The scripts here are bash for this reason.
- No emojis. Keep entries skimmable; the user is triaging, not reading.

## Extending with more collectors (design each in its own session)

- OmniFocus: query via `osascript`; needs agreement on which perspectives count as WIP.

## Stable commands to allowlist

- `Bash(bash /Users/jml/.claude/skills/wip/scripts/inventory.sh:*)`
- `Bash(bash /Users/jml/.claude/skills/wip/scripts/sessions.sh:*)`
- `Bash(bash /Users/jml/.claude/skills/my-prs/scripts/run.sh:*)` (shared with my-prs)
- `Bash(git worktree prune:*)`, `Bash(gh pr view *)`, `Bash(gh repo view *)`
