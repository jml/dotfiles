---
name: chase
description: Lists the Slack review requests, across all of jml's rollouts, that have gone unanswered too long, and drafts the follow-up for each. Use when jml says "/chase", "who do I need to chase", "which reviews are overdue", or "what review requests need nudging".
---

# Chase

Every review request a rollout session sends is logged in `~/.local/state/rollouts/requests.jsonl`. This skill reads that log for every rollout at once and reports the requests to chase.

## Steps

1. Run `python3 ~/.claude/skills/rollout/scripts/rollout.py chase --json`. Each entry is a PR whose latest request has waited past its threshold without a human reviewing or commenting since. Weekday hours count day and night, UTC. The threshold grows with PR size (`chase` in `~/.claude/skills/rollout/config.json`).
2. If the list is empty, say so in one line and stop.
3. Group the entries by person or team, then by channel, so one nudge can cover several PRs for the same reviewer. For each group, propose the next move, escalating from what `channels_tried` shows was already tried:
   - first chase: a reply in the original thread (`permalink`), or the same channel if there's no permalink;
   - second: a DM to a requested user (`requested_users`), or to a member of the requested team who reviewed in that repo recently (`gh search prs --repo <repo> --reviewed-by …` or the PR's past reviews);
   - third: ask jml whether to go to the team lead or the team channel with a direct ask.
4. Draft each nudge in jml's voice (jml-voice skill): short, a list of `:pr: [<title>](<url>)` lines when it covers several PRs, and no "Written by Claude" line (Slack labels messages sent through Claude). Show all drafts in one message and ask which to send.
5. Send each approved nudge with `slack_send_message` (thread replies take `thread_ts`), then log it: `rollout.py record-request --rollout <rollout> --pr <pr> --channel "<channel or DM login>" --permalink <link>`. This restarts that PR's clock.

When a channel or bot account in the output is wrong (a bot's comment counted as an answer, a team mapped to the wrong channel), fix `config.json`: bot logins go in `chase.ignore_authors`, channel mappings in `channels`.
