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
| `~/.claude/skills/rollout/config.json` | Deploy workflows per repo (`deploys`, the full deploy, and `stage_deploys`, the single-stage dispatch), Slack channel map, chase thresholds, and `review_gated_checks`: checks that resolve only on approval (`doc-approval`), which status reports as `awaiting_review` and leaves out of a PR's CI state; `me`, jml's GitHub login: a PR someone else wrote gets no actions, only `author` and a waiting phase, because its author rebases, answers, requests review and merges it |

Script: `python3 ~/.claude/skills/rollout/scripts/rollout.py` with `status <name> [--json]`, `wait-checks <owner/repo#N>`, `mark <name> <node> [--undo]`, `record-request --rollout --pr --channel [--permalink] [--sent-at]`, `chase`, `list`.

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
- PR `stages`: the Terraform stages the PR deploys through, as `cimatrix provision` lists them (`["400-merge-bot", "400-vpa-bot"]`). With it, `deployed:<env>` holds once every stage is deployed at a commit that includes the PR, by a full deploy whose job for that stage succeeded or by a single-stage dispatch; without it, only a fully successful deploy counts. Set it on every mono PR node that a `deployed:` edge names, because a full deploy often fails on stages the PR never touched.
- Step `by`: `jml` (default), `external` (someone else; jml may need to ask them), `session` (this session may do it), `automatic` (a schedule or bot does it; only its `check` can complete it).
- Step `check`: `{"workflow": {repo, file, event?, since?}}` passes on a successful run; `{"variable": {repo, name}}` passes once the Actions variable exists. Without a check, a step is done only when marked.
- `channel`: a Slack channel for every review request in this rollout (a `proj-` channel, say). It beats the config map.
- `context`: the design or intent docs. Read them before recommending an answer to any decision.
- `work` (instead of `pr`): a change not yet opened as a PR. `session` names the session building it. Add `pr` once the PR exists; keep `work` as the note of what it is.
- `proposed: "<why>"`: new work this session found that jml hasn't yet accepted into the rollout. It blocks whatever waits on it until jml decides. See "New work" below.
- `follow_ups`: `[{"ref", "url", "title"}]`, work jml chose to do after the rollout, listed on the status page.

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
4. If anything changed since the last tick and the Artifact is on, publish the status page (see Status page below). The local copy updates on its own.
5. Schedule the next tick with `ScheduleWakeup`, `delaySeconds` = `next_tick_seconds` (60–3600), passing the same `/rollout <name>` prompt. When `complete` is true, stop the loop, tell jml, and offer to archive the session.

### Waiting on CI you started

Whenever this session opens a PR or pushes to one (a fix, a rebase, an answer to review), start

    python3 ~/.claude/skills/rollout/scripts/rollout.py wait-checks <owner/repo#N>

with the Bash tool's `run_in_background: true`. It exits once the PR's CI and its review bots have finished, ignoring checks that clear only on approval, or after an hour. You're woken when it exits: run a tick then, without waiting for the scheduled wakeup, so a green PR is marked ready and its review requested within about a minute. Start at most one per PR; keep the scheduled wakeup as the fallback.

### Review bots before people

A PR goes to a person only once its review bots are finished with it. `status` reports `bot_review` on each open PR: `pending` (a bot review check such as `reviewer/<team>` or `claude-code-action` is running), `findings` (a thread a bot started is unresolved), `approved` (the `Checks review gate` passed, so Checks' approving review is on its way), `done` (the bots finished without approving) or absent (no bot reviews this repo). It offers `request-review` only for `done` or absent. So address bot findings first, as part of `respond`, and wait for the bots' next pass after each push: in repos where Checks approves, that pass may approve the PR and no person needs asking.

### Auto actions

- **rebase** (`auto` only when the PR's dependencies have landed): rebase onto the default branch in the PR's worktree and push with the explicit `--force-with-lease=<branch>:<sha>` form from jml's CLAUDE.md. When `auto` is false, rebase locally and don't push: later legs get rebased again after each merge, and every push re-runs CI.
- **fix-ci**: follow the fix-ci skill. A re-run replays the original run's workflow file and merge ref, so if main has moved or the workflow's auth changed since, push (or update the branch) rather than re-run. If the failure also appears on main or on unrelated PRs, it isn't this PR's: find the fix PR if there is one, and bring jml the choice "wait and rebase onto the fix, or land the fix" with that evidence.
- **respond**: fetch the feedback (fetch-pr-feedback skill) and sort each item; `detail` counts bot threads and people's threads. Mechanical (typo, rename, missing test the reviewer named): fix and push, and resolve a bot's thread once its finding is fixed so its next pass sees it addressed. Judgment (follow-up or fix now, fix at a lower layer, scope): ask jml, with a recommended answer and the reason. Decline: draft the reply. Post no reply until jml approves it; post approved replies in the thread with `gh api`.

- **mark-ready** (jml's standing yes, 2026-11-18): mark the green draft ready with `gh pr ready --repo <repo> <N>`. A node with `"hold_draft": true` is a draft on purpose and gets no action. A draft whose `after` names another PR node that hasn't merged or deployed as required gets no mark-ready either.
- **mark-draft** (jml's standing yes, 2026-11-24): a PR in review whose `after` names another PR node that hasn't merged or deployed as required goes back to draft with `gh pr ready --undo --repo <repo> <N>`. It is rebased once that lands, which changes its code and voids approvals of the old head, so a review now would be wasted. It gets no review request while blocked, and goes ready again through mark-ready once the dependency lands. Say in the tick's report which PRs went back to draft and what each waits on.
- **request-review** (jml's standing yes, 2026-11-18, when `detail.channels` isn't empty): send `detail.message`, exactly `:pr: [<title>](<url>)` with no attribution line (Slack labels messages sent through Claude), to the first channel in `detail.channels` with `slack_send_message`, using the channel's ID from `config.json` `channels.ids` or `slack_search_channels`. Then `record-request` with the message link the tool returns, and tell jml in the tick's report where it went.

### Actions jml decides

- **request-review** with empty `channels` (`auto: false`): ask jml which channel, send the request there, and add the mapping to `config.json` (`channels.teams` keyed by the requested GitHub team, or `channels.paths`) so the next one resolves itself. `#chan-lib` and `proj-` channels go in `paths` or the manifest's `channel`, not `teams`.
- **merge**: ask, then `gh pr merge --repo <repo> <N>` with that repo's merge method (squash for chainguard-dev repos unless jml says otherwise).
- **do-step**: `by: jml` or `external`: say what's needed and from whom; when jml says it's done, `rollout.py mark`. `by: session`: ask once, then do it.
- **check-failures** (failing while dependencies are unlanded): say whether the failures are the expected ones from the PR body; ask only if they aren't.
- **decide** (a PR closed unmerged): ask whether to drop or replace the node.

### Deploying a stage by dispatch

**dispatch-stage** (jml's standing yes, 2026-11-24, when every check below passes; when one fails or can't be answered, put the evidence to jml and ask). A `deployed:<env>` edge can wait for hours on a full deploy that won't carry the PR soon: enforce.dev deploys in full only when someone pushes a `v*` release tag, a busy main cancels queued chainops.dev deploys before they start, and a full deploy can fail on stages the PR never touched. When a PR node with `stages` has merged and some stage hasn't reached `<env>`, deploy that stage with the env's `stage_deploys` workflow:

    gh workflow run <stage_deploys[env]> --repo <repo> -f stage=<stage> -f checkout_ref=<full sha> -f reason="<rollout name>: <PR URL>"

It is safe only when all of these hold:

- **The commit.** `checkout_ref` is the full SHA of a commit on main that includes the PR's merge commit; the enforce.dev workflow refuses anything else. For prod, use the commit staging last applied the stage at, so prod runs what staging already ran.
- **Staging first.** For prod, the stage applied successfully in staging at that commit or later, and the service it deploys shows no new errors in staging since then (its error logs, and its alerts if it has any).
- **What rides along.** List the commits that touch the stage between what it last deployed and `checkout_ref`: `git log <deployed>..<sha> -- env/<env>/iac/<stage>` plus the modules and Go code it builds. The stage workflow's run title (`<stage> @ <sha>`) or the last full deploy tells you what it last deployed. Each of those commits must already be in staging. Name any that belong to other people in the tick's report.
- **The gates.** The stage is one `.github/scripts/validate-dispatch-stage.sh` accepts (2xx, 4xx or 998 for enforce.dev), `.github/release-block` doesn't hold it, and its last full-deploy job didn't fail for a reason a dispatch would repeat. Never pass `break_glass`.
- **Order.** Dispatch one stage at a time, in dependency order (a stage whose outputs another reads goes first), and wait for each run to succeed before the next.

Then watch the run in the background (`gh run watch <id> --repo <repo> --exit-status`) and run a tick when it exits. Record each dispatch in `<name>.decisions.md` (stage, commit, run URL). The prod workflow announces itself in #eng-oncall, so say in the tick's report that it went out.

Record every answer in `<name>.decisions.md` as a dated line: the question, the answer, and the rule it implies if jml states one ("bot style nits are follow-ups"). Apply those rules on later ticks without asking again.

## New work

Following a rollout turns up work nobody planned: a ruleset change, a missing grant, a fix on main, a better place for a fix. Whether it belongs in the rollout is jml's call. The mechanics after that are the session's.

1. Add it to the manifest as a node with `proposed` saying what was found and why it matters, and `after` edges from whatever it blocks. Use `work` for a change, or `step` for something a person does. `status` turns it into a `decide-scope` action.
2. Put the decision to jml with a recommendation: **part of this rollout** (it blocks something here), **follow-up** (it doesn't block; do it later), **fix it elsewhere** (it belongs at a lower layer or to another team), or **drop it**.
3. Then:
   - Part of the rollout: remove `proposed`. If the change is a handful of lines in one file, make it here and add the PR as `pr`. Otherwise propose a session for it (`start_session`, own worktree, brief covering only that change), record the session's id as `session` on the node, and add `pr` when its PR opens.
   - Follow-up: file a Linear issue (the go-linear tools; link the rollout's Linear issue as related), move the node to `follow_ups`, and remove its edges.
   - Elsewhere: file the issue or message for the owning team, and keep the node as a `by: external` step if it still blocks this rollout.
   - Drop: remove the node.
4. Record the decision in `<name>.decisions.md`.

## Status page

Each rollout has a page jml opens to see the whole thing: **Needs you** (only jml's own moves: approvals and decisions), **With other people** (steps someone else does, other people's PRs, review requests out and unanswered), the dependency graph with every node's state, what the session is on, and what's in flight. `core.whose_move` decides which list an action goes in. `--page` renders it; publish it with the Artifact tool.

Every `status` run also writes a local copy that reloads itself every two minutes, `~/.local/state/rollouts/<name>.html`, and an index of all rollouts, `~/.local/state/rollouts/index.html`. Neither needs an approval, and they are jml's view of the rollout: give jml the `file://` link to the index when the rollout starts.

Publish the Artifact only when `config.json` has `"publish_artifact": true` (jml turned it off on 2026-11-18; skip the rest of this section while it's off). jml's organization's managed settings ask before every Artifact read and publish, so each publish costs jml an approval.

- First publish (no `<name>.artifact` yet): publish the rendered file with `icon: "flowchart"` and a one-sentence `description`, then write the URL it returns to `<name>.artifact`, and give jml the link.
- Later publishes: pass the URL from `<name>.artifact` as `url`. In a session that hasn't yet read or published it, read it first (`action: "read"`), then publish the new render with that `url`; the page is generated, so there is nothing on it to merge.
- Republish only on a tick where some node's state or actions changed, so the page's version history marks real changes.

## Rules

- Never post to Slack or GitHub on jml's behalf without their yes for that message, except the templated review request above, which jml gave a standing yes.
- Never merge or dispatch a workflow without asking, unless this skill (stage dispatch, above) or `<name>.decisions.md` records a standing yes for that kind of action.
- Don't edit the manifest's edges silently; propose the change and say why.
- Report in short lines: what changed since the last tick, then what needs jml. Say nothing on a tick where nothing changed.
