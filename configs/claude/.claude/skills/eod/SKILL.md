---
name: eod
description: End-of-day sweep of jml's open PRs that belong to no rollout. Sends the review requests nobody sent, marks green drafts ready, offers the overdue chases and stale-draft closes, then writes a note for tomorrow listing what jml is waiting on and what's jml's move. Use when jml says "/eod", "end of day", "wrap up my PRs", or "what am I waiting on before I stop".
---

# End of day

Run this before jml stops for the day. The sweep gets review requests out every evening, so a day spent on something else still leaves PRs in front of reviewers; the note for tomorrow is a by-product. It never posts the note to Slack. PRs in a rollout manifest belong to their rollout session, so this leaves them alone and only counts them.

Script: `python3 ~/.claude/skills/rollout/scripts/rollout.py`, shared with the rollout and chase skills. State is in `~/.local/state/rollouts/`: `requests.jsonl` (every review request sent, shared with `/rollout`), `eod.holds.json` (PRs not to ask about until a date) and `eod.jsonl` (one line per run, for judging whether `/eod` works).

## Steps

1. Run `rollout.py sweep --json --log` and `rollout.py chase --json`. The sweep puts each PR in one `bucket`, the first of these that fits:

   | Bucket | Meaning | What `/eod` does |
   |---|---|---|
   | `held` | jml held it until `detail.until` | nothing |
   | `reply` | a person's thread waits on jml, or changes were requested | lists it |
   | `blocked` (in review) | a PR in review stacked on another open PR (`detail.builds_on`) or whose base isn't the default branch (`detail.base`) | puts it back to draft (step 3) |
   | `fix` | failing CI (`detail.failing`), a conflict, or a review bot's open finding | lists it |
   | `stacked` | only `design_doc_checks` fail (`pr-hygiene`), and another open PR in the repo adds its `Design-Doc:` path (`detail.design_doc`) | lists it; the checks pass once the doc merges |
   | `merge` | approved and green | lists it |
   | `owned` | would be `ask`, but a live session works on it (`detail.session`) | lists it; that session sends the request |
   | `ask` | green and ready, review bots done, no request sent | sends the request (step 3) |
   | `blocked` | a green draft that can't merge yet: a live session works on it (`detail.session`), it builds on another open PR (`detail.builds_on`), its base isn't the default branch (`detail.base`), or its description has unticked "To-do before merge" items (`detail.todos`) | lists it, and never offers to mark it ready |
   | `ready` | a green draft that could merge now | marks it ready (step 3) or asks (step 4) |
   | `bots` | review bots running, or Checks' approval on its way | lists it |
   | `overdue` | a request has waited past its chase threshold | offers a chase (step 4) |
   | `waiting` | a request was sent and isn't overdue | lists it |
   | `running` | CI is still running | lists it |
   | `parked` | none of the above | lists it |

   `stale: true` marks a draft with no commit, comment or review by a person in `eod.stale_draft_days` days (`rollout/config.json`).

2. **Backfill.** jml sometimes asks for review by hand. Before sending anything for an `ask` PR, search Slack with `slack_search_public_and_private`: keywords `["\"<repo>/pull/<N>\""]`, filter `from:<@U08S0H4L5KJ>`. For each hit, run `rollout.py record-request --rollout eod --pr <owner/repo#N> --channel <channel> --permalink <link> --sent-at <UTC time>` and send nothing for that PR. Leave it out of the note's `ask` lines; it is waiting.

3. **Under jml's standing yes** (given 2026-11-18 for `/rollout`, extended to `/eod` 2026-11-23), do these without asking:
   - **`request-review` with `auto: true`:** send `detail.message` exactly, `:pr: [<title>](<url>)` with no attribution line (Slack labels messages sent through Claude), to the first channel in `detail.channels`. Use `slack_send_message` with the channel's ID from `config.json` `channels.ids`, or from `slack_search_channels`. Then run `rollout.py record-request --rollout eod --pr <owner/repo#N> --channel <channel> --permalink <link the tool returns>`.
   - **`mark-draft`** (standing yes, 2026-11-24): run `gh pr ready --undo --repo <owner/repo> <N>`. The PR is rebased once what it builds on merges, so a review now would be wasted; the `blocked` draft rules keep it a draft until then.
   - **`mark-ready` with `auto: true`** (drafts opened since the first `/eod`), unless the draft is stale: run `gh pr ready --repo <owner/repo> <N>`. Its review request must wait for the review bots' pass on the now-ready PR, so start `rollout.py wait-checks <owner/repo#N>` with `run_in_background: true`. When it exits, run the sweep again and handle that PR's `ask` the same way. If the wait outlasts the session, the next `/eod` picks it up.

4. **Ask jml the rest** in one `AskUserQuestion` call with at most four questions, leaving out any with nothing to ask. Each question has at most four options, so take the oldest four. The rest come up on the next run.
   1. **Where should these review requests go?** `ask` PRs whose `detail.channels` is empty. Offer the channels of the PR's requested teams, if `~/.config/rollout/local.json` maps any, and the team's usual channel if you know it. Send to the answer as in step 3. If jml names a channel for a team or repo, offer to add it to `channels` in `~/.config/rollout/local.json`.
   2. **Mark these drafts ready?** (multi-select) `ready` PRs with `auto: false` that aren't stale: drafts older than the first `/eod`, which may be drafts on purpose. The question says that unticked drafts will be held for `eod.hold_days` days. Hold each with `rollout.py hold <owner/repo#N> --reason "draft on purpose"`.
   3. **Send these chases?** (multi-select) Draft the nudges as the chase skill does, grouped by reviewer, and send the ones jml ticks, as that skill says. This covers rollout PRs too: the chase list spans every request in `requests.jsonl`.
   4. **Close these stale drafts?** (multi-select) PRs with `stale: true`. Close each ticked one with `gh pr close --repo <owner/repo> <N>`. Hold the unticked ones (`--reason "stale draft kept"`), and say so in the question.

5. **Write the note** to `~/.local/state/rollouts/eod/<YYYY-MM-DD>.md`, creating the directory if needed. Then give it in the closing message, after a line for each thing step 3 or 4 sent, marked ready, closed or held, with where it went. One line per PR, `[<repo>#<N> — <title>](<url>)` followed by the specific thing, never a generic "needs attention":
   - **Your move:** `reply` (who is waiting and on what), `fix` (the failing check, the conflict or the bot finding), `merge`, and any `ask` or `ready` left unsent or unmarked.
   - **Waiting on others:** `waiting` (the channel and how long ago), `overdue` that jml chose not to chase, `bots`, `owned` (which session will send the request), and `stacked` (which design-doc PR it waits on).
   - **Parked:** `held` (until when, and why), `blocked` (which session, which PR it builds on, or which to-do is open), `running` and `parked`.
   - One closing line: how many PRs were left to rollouts, by rollout (`in_rollouts`), and any PRs in archived repos (`archived`), which can't be closed and need the repo unarchived or deleted.

## Whether it's working

`eod.jsonl` has one line per run: the counts per bucket before any action, how many hours the oldest `ask` PR had waited since its last commit (`oldest_ask_hours`), and the number of PRs in rollouts. When jml asks whether `/eod` is worth keeping, read it alongside `requests.jsonl` entries with `"rollout": "eod"`:

- the `ask` count left after each run, which should be 0 most evenings;
- `oldest_ask_hours`, which should drop below 24;
- working days with one merge or fewer, from `gh search prs --author @me --merged-at …`, which should become rarer than in 9–20 Nov 2026, when merges averaged 6.4 per working day.

Suggest dropping `/eod` if jml skipped it three of five working days in week two. Suggest the same if the `ask` count before the sweep stays at 0 for two weeks, since the habit would then have formed without it.
