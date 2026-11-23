---
name: standup
description: Prepares jml's report for the Developer Platform Foundations pod standup. Brings jml's Linear issues up to date, offers the review requests nobody sent, then lists blockers and dependencies on people outside the subteam, each split into new since the last standup and standing, and posts async notes to the pod's private channel on jml's word. Use when jml says "/standup", "prep my standup", "what are my blockers", or "what do I say at standup".
---

# Standup

The pod's standup is for unblocking. Every open PR of jml's counts as a blocker (How We Work), and a pod member asked for dependencies on other teams and subteams too. This skill reports both, split into what is new since the last standup and what is standing, and posts the same text to the pod's private channel as async notes.

It asks before every outward action: sending a review request, marking a draft ready, changing Linear, and posting. It never reads Slack for dependencies.

Script: `python3 ~/.claude/skills/rollout/scripts/rollout.py`, shared with `/rollout`, `/chase` and `/eod`. Config: the `standup` section of `~/.claude/skills/rollout/config.json` (subteam and pod members, the channel, the subteam's Linear team and the home GitHub team). State: `~/.local/state/rollouts/standup/<date>.json` (each standup's blockers and dependencies), `standup/<date>.md` (the notes) and `standup.jsonl` (one line per run).

People are **subteam** (jml, Ada Example, Cy Example), **pod** (the subteam plus Bo Example and Di Example) or **other**. A dependency is anything between jml and someone outside the subteam.

## Steps

1. **Gather Linear facts** into `$SCRATCH/standup-linear.json` (your scratchpad), as `{"issues": [...]}` with one object per issue: `id` (the identifier, such as `DEV-123`), `title`, `url`, `status`, `status_type`, `assignee` (display name or null), and, for jml's issues, `blocked_by` and `blocks` (lists of identifiers).
   - jml's open issues: Linear MCP `list_issues` with `assignee: "me"`, once with `state: "started"` and once with `state: "unstarted"`, fields `id,title,url,status,statusType,assignee`.
   - Relations: `go-linear issue list --assignee=me` with `--has-blocked-by` and again with `--has-blocking`. For those issues and every issue whose status is Blocked, call Linear MCP `get_issue` with fields `["status","relations"]`. `go-linear issue get` drops nested relation fields, so don't use it for this.
   - The other end of each relation: `get_issue` with fields `["title","url","status","statusType","assignee"]` for every related issue not already gathered. Add it to `issues` without `blocked_by` or `blocks`.
2. **Run** `rollout.py standup --linear $SCRATCH/standup-linear.json --no-save`. Its JSON has `blockers`, `dependencies`, `drift` and `fixed_in_passing`. Each PR blocker carries the `/eod` sweep's `bucket`, `detail` and `actions`. PRs in a rollout have `rollout` set and no actions: their sessions own them.
3. **Ask jml** in one `AskUserQuestion` call, at most four questions with at most four options each (oldest first; the rest wait for the next run), leaving out any with nothing to ask:
   1. **Send these review requests?** (multi-select) PR blockers with a `request-review` action. Each option names the channel from `detail.channels`; a PR with no channel asks for one instead. Before offering, search Slack for a request jml sent by hand, as `/eod` step 2 does, and record it instead of offering it.
   2. **Mark these drafts ready?** (multi-select) PR blockers with a `mark-ready` action.
   3. **Update these Linear issues?** (multi-select) `drift` entries on jml's own issues. Each option states the change: the status to set (`propose`), or the blocked-by relation to add, naming the blocker when the issue's title or description says what it is.
   4. **Change these issues of other people's?** (multi-select) A relation that touches someone else's issue, or a comment on one, with the drafted text in the option. Comments go through Linear MCP `save_comment`; jml reviews the text in the option, so it carries no attribution line.
4. **Do what jml ticked.** Send each review request with `slack_send_message` exactly as `:pr: [<title>](<url>)` and record it with `rollout.py record-request --rollout standup --pr <ref> --channel <channel> --permalink <link>`. Mark drafts ready with `gh pr ready`. In Linear, set status by the state's UUID (`go-linear state list` for the issue's team; names are ambiguous across teams), add relations with `go-linear issue relate --type=blocks` using UUIDs, blocker first, and update the facts file to match.
5. **Run** `rollout.py standup --linear $SCRATCH/standup-linear.json`, which saves today's snapshot. If it says `baseline: true`, this is the first run: everything is standing, and the notes say so in one line.
6. **Write the notes** to `~/.local/state/rollouts/standup/<date>.md` and show them. They go out under jml's name, so write them in jml's first person (jml-voice skill). Use Markdown, as `/eod`'s review requests do: one line per item, a link first, then the specific state, never "needs attention":

   ```
   **Blockers** (2 new, 9 standing, 1 cleared)
   New
   • [repo#N — title](url): `dbt Cloud` failing; my move
   • [TEAM-N — title](url): Blocked, and the blocker isn't recorded
   Standing
   • [repo#N — title](url): waiting on data-analytics-admins in #chan-data-eng since 22 Nov, overdue
   • [repo#N — title](url): held until 30 Nov, a draft on purpose
   Cleared
   • repo#N — title

   **Dependencies** (1 new, 1 changed, 3 standing, 0 resolved)
   New
   • [TEAM-N — my issue](url) waits on [TEAM-N](url) (Name, TEAM; other team)
   Changed
   • [TEAM-N — my issue](url) waits on [TEAM-N](url) (Name, TEAM; pod): Todo → In Progress

   **Fixed in passing**
   • [repo#N — title](url)
   ```

   - **Blockers:** describe each PR from its `bucket` and `detail` as `/eod` does (the failing check, who it waits on and since when, overdue, held until when, in which rollout). An issue blocker names what it waits on (`waiting_on`), or says the blocker isn't recorded (`unrecorded`).
   - **Dependencies:** say who waits on whom: `waits_on` is jml waiting, `waited_on_by` is someone waiting on jml. Name the counterpart's `label` and whether it is in the pod or another team. A `changed` entry shows `was` → `state`.
   - **Fixed in passing:** PRs merged since the last standup that name no Linear issue. Leave the section out when it's empty.
   - Leave out an empty New, Standing, Changed, Cleared or Resolved heading, but keep the counts.
7. **Post on jml's word.** Ask whether to post to `standup.channel`. On yes, send everything down to the Standing headings as the top-level message, then each Standing list as a reply in its thread, with `slack_send_message` (`channel_id` from `standup.channel_id`). Add no attribution line: Slack labels messages sent through Claude. Append the permalink to the notes file.
8. **Log** with `rollout.py standup-log --offered <drift entries offered> --applied <applied> --posted|--no-posted`.

## Whether it's working

`standup.jsonl` has one line per run. After two weeks, read it with jml:

- `drift_offered` should fall as Linear stays current. If it stays high, the rules propose changes jml doesn't want; read which ones jml declines.
- Ask the pod whether the dependency list tells them what they need. If it doesn't, ask what is missing before adding Slack as a source.
