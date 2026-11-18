---
name: rollout
description: Shepherd one multi-PR change (a rollout) to done across repos — track its DAG of PRs and manual steps, rebase, fix CI, triage review feedback, wait for deploys, and queue Slack review requests for jml's approval, escalating only decisions. Use when jml says "/rollout <name>", "shepherd this change", "drive these PRs to done", "start a rollout for DEV-1234", or asks to set up a rollout manifest for a set of PRs. For overdue review requests across rollouts, use the chase skill.
argument-hint: "<rollout-name> | new <name> <Linear-ID or PR URLs>"
---

# Rollout

A rollout is one conceptual change that lands as several PRs, possibly across repos, plus steps that aren't PRs (a hand-applied bootstrap, a workflow dispatch, a decision). This skill runs in a session of its own, one per rollout, on a self-paced `/loop`. The session holds no state of its own: every tick rebuilds the picture from GitHub and the files below, so the session can be killed and restarted at any time.

The goal is that jml makes decisions and approves outward messages; the session does the mechanics.

## Files

| Path | What |
|---|---|
| `~/.local/state/rollouts/<name>.json` | Manifest: the nodes and their `after` edges |
| `~/.local/state/rollouts/<name>.marks.json` | Steps marked done (`rollout.py mark`) |
| `~/.local/state/rollouts/<name>.decisions.md` | jml's decisions, read every tick as policy |
| `~/.local/state/rollouts/requests.jsonl` | Every Slack review request, all rollouts (chase reads it) |
| `~/.local/state/rollouts/<name>.artifact` | URL of the rollout's status page (an Artifact) |
| `~/.claude/skills/rollout/config.json` | Deploy workflows per repo, Slack channel map, chase thresholds, and `review_gated_checks`: checks that resolve only on approval (`doc-approval`), which status reports as `awaiting_review` and leaves out of a PR's CI state; `me`, jml's GitHub login: a PR someone else wrote gets no actions, only `author` and a waiting phase, because its author rebases, answers, requests review and merges it |

Script: `python3 ~/.claude/skills/rollout/scripts/rollout.py` with `status <name> [--json]`, `mark <name> <node> [--undo]`, `record-request --rollout --pr --channel [--permalink] [--sent-at]`, `chase`, `list`.

## Manifest

```json
{
  "name": "git-history-loader",
  "title": "One line saying what the change is",
  "linear": ["DATA-558"],
  "context": ["https://github.com/chainguard-dev/mono/pull/84717"],
  "channel": null,
  "nodes": [
    {"id": "infra", "pr": "chainguard-dev/data-analytics-infra#21", "after": ["bootstrap"]},
    {"id": "bootstrap", "step": "A project Owner applies bootstrap/ by hand", "by": "external"},
    {"id": "first-run", "step": "Scheduled run succeeds", "by": "automatic",
     "after": ["infra@deployed:prod"],
     "check": {"workflow": {"repo": "o/r", "file": "x.yml", "event": "schedule"}}},
    {"id": "models", "pr": "o/r#517", "after": ["first-run"], "hold_draft": false}
  ]
}
```

- `after` entries are `<node>` or `<node>@<condition>`. PR conditions: `merged` (default), `deployed:<env>`, where `<env>` must have a workflow in `config.json` `deploys` for that repo (else the edge reads as unknown). Step condition: `done`.
- Step `by`: `jml` (default), `external` (someone else; jml may need to ask them), `session` (this session may do it), `automatic` (a schedule or bot does it; only its `check` can complete it).
- Step `check`: `{"workflow": {repo, file, event?, since?}}` passes on a successful run; `{"variable": {repo, name}}` passes once the Actions variable exists. Without a check, a step is done only when marked.
- `channel`: a Slack channel for every review request in this rollout (a `proj-` channel, say). It beats the config map.
- `context`: the design or intent docs. Read them before recommending an answer to any decision.

Edges exist only where deploy safety or ordering requires them. A PR whose only edge to its parent is `merged`, in the same repo, is a candidate to fold into the parent; say so when building or reviewing a manifest.

## Starting a rollout (`/rollout new <name> …`)

Usually the session that built the PRs hands off here once the first PR is pushed. The handing-off session should pass the PR list and the ordering constraints it knows.

1. Find the PRs: those given, plus `gh search prs <LINEAR-ID> --owner chainguard-dev --author @me --state open`, plus PRs whose bodies link the design doc. A Linear issue often spans more than one rollout, so confirm membership with jml rather than taking every hit.
2. Read each PR body for ordering: "To-do before merge", "stacked on", "doesn't work until X is applied", apply-order notes. Turn each into an `after` edge or a step node.
3. Backfill requests already sent: for each PR, search Slack with `slack_search_public_and_private`, keywords `["\"<repo>/pull/<N>\""]`, filters `from:<@U08S0H4L5KJ>`, and `record-request --sent-at <UTC time>` for each hit. Note any team→channel pairing it reveals in `config.json`.
4. Write the manifest, run `status`, and show jml the DAG and the status in one message for confirmation. Then start the loop (`/loop /rollout <name>`).

## Each tick (`/rollout <name>`)

1. Read `<name>.decisions.md`.
2. Run `rollout.py status <name> --json --page <scratchpad>/<name>.html`. Its `nodes[].actions` say what each node needs; `auto: true` means this session does it without asking. `--page` also writes the status page.
3. Do the `auto` actions, then put every non-auto action to jml (below), in one `AskUserQuestion` call with up to four questions. Group them, most blocking first; don't ask one per tick.
4. Publish the status page if anything changed since the last tick (see Status page below).
5. Schedule the next tick with `ScheduleWakeup`, `delaySeconds` = `next_tick_seconds` (60–3600), passing the same `/rollout <name>` prompt. When `complete` is true, stop the loop, tell jml, and offer to archive the session.

### Review bots before people

A PR goes to a person only once its review bots are finished with it. `status` reports `bot_review` on each open PR: `pending` (a bot review check such as `reviewer/<team>` or `claude-code-action` is running), `findings` (a thread a bot started is unresolved), `approved` (the `Checks review gate` passed, so Checks' approving review is on its way), `done` (the bots finished without approving) or absent (no bot reviews this repo). It offers `request-review` only for `done` or absent. So address bot findings first, as part of `respond`, and wait for the bots' next pass after each push: in repos where Checks approves, that pass may approve the PR and no person needs asking.

### Auto actions

- **rebase** (`auto` only when the PR's dependencies have landed): rebase onto the default branch in the PR's worktree and push with the explicit `--force-with-lease=<branch>:<sha>` form from jml's CLAUDE.md. When `auto` is false, rebase locally and don't push: later legs get rebased again after each merge, and every push re-runs CI.
- **fix-ci**: follow the fix-ci skill. A re-run replays the original run's workflow file and merge ref, so if main has moved or the workflow's auth changed since, push (or update the branch) rather than re-run. If the failure also appears on main or on unrelated PRs, it isn't this PR's: find the fix PR if there is one, and bring jml the choice "wait and rebase onto the fix, or land the fix" with that evidence.
- **respond**: fetch the feedback (fetch-pr-feedback skill) and sort each item; `detail` counts bot threads and people's threads. Mechanical (typo, rename, missing test the reviewer named): fix and push, and resolve a bot's thread once its finding is fixed so its next pass sees it addressed. Judgment (follow-up or fix now, fix at a lower layer, scope): ask jml, with a recommended answer and the reason. Decline: draft the reply. Post no reply until jml approves it; post approved replies in the thread with `gh api`.

### Actions jml decides

- **request-review**: show the drafted message (`detail.message`, exactly `:pr: [<title>](<url>)`, with no attribution line: Slack labels messages sent through Claude) and the candidate channels (`detail.channels`, most specific first). On a yes, send it with `slack_send_message` to the channel's ID (`config.json` `channels.ids`, or look it up with `slack_search_channels`), then `record-request` with the message link the tool returns. If `channels` is empty, ask jml which channel and add the mapping to `config.json` (`channels.teams` keyed by the requested GitHub team, or `channels.paths`) so the next request resolves itself. `#chan-lib` and `proj-` channels go in `paths` or the manifest's `channel`, not `teams`.
- **merge**: ask, then `gh pr merge --repo <repo> <N>` with that repo's merge method (squash for chainguard-dev repos unless jml says otherwise).
- **mark-ready**: ask; a draft may be draft on purpose. If jml says it stays draft, set `"hold_draft": true` on the node.
- **do-step**: `by: jml` or `external`: say what's needed and from whom; when jml says it's done, `rollout.py mark`. `by: session`: ask once, then do it.
- **check-failures** (failing while dependencies are unlanded): say whether the failures are the expected ones from the PR body; ask only if they aren't.
- **decide** (a PR closed unmerged): ask whether to drop or replace the node.

Record every answer in `<name>.decisions.md` as a dated line: the question, the answer, and the rule it implies if jml states one ("bot style nits are follow-ups"). Apply those rules on later ticks without asking again.

## Status page

Each rollout has a page jml opens to see the whole thing: what needs them, the dependency graph with every node's state, and what's in flight. `--page` renders it; publish it with the Artifact tool.

- First publish (no `<name>.artifact` yet): publish the rendered file with `icon: "flowchart"` and a one-sentence `description`, then write the URL it returns to `<name>.artifact`, and give jml the link.
- Later publishes: pass the URL from `<name>.artifact` as `url`. In a session that hasn't yet read or published it, read it first (`action: "read"`), then publish the new render with that `url`; the page is generated, so there is nothing on it to merge.
- Republish only on a tick where some node's state or actions changed, so the page's version history marks real changes.

## Rules

- Never post to Slack or GitHub on jml's behalf without their yes for that message.
- Never merge, mark ready or dispatch a workflow without asking, unless `<name>.decisions.md` records a standing yes for that kind of action in this rollout.
- Don't edit the manifest's edges silently; propose the change and say why.
- Report in short lines: what changed since the last tick, then what needs jml. Say nothing on a tick where nothing changed.
