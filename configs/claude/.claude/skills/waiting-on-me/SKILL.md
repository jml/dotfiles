---
name: waiting-on-me
description: Lists everyone waiting on jml right now, across GitHub, Slack and Linear, so jml can unblock them. Covers PRs that request jml's review by name, PR and issue comments that @-mention jml since jml last replied there, Slack mentions and DMs that ask jml for something, and Linear mentions and assignments. Every item links to the ask. Use when jml asks "who's waiting on me", "anything where anyone is waiting on me", "could I unblock someone", "what do people need from me", or says "/waiting-on-me". Takes "days=N" to widen the 14-day window.
model: sonnet
effort: medium
context: fork
background: false
---

# Waiting on me

This skill answers one question: who is blocked on jml? It doesn't cover jml's own PRs (`/eod` and `/my-prs` do), and it reads only. It sends nothing, approves nothing and marks nothing read.

`$DAYS` is 14 unless jml passed `days=N`. `$SINCE` is today minus `$DAYS`, as `YYYY-MM-DD`. jml's Slack user ID is `U08S0H4L5KJ`.

## How an item clears

| Source | Clears when jml… | Also drops when |
|---|---|---|
| GitHub review request | reviews it, or removes the request | — |
| GitHub @-mention | comments or reviews on that PR or issue after the mention | the PR or issue is closed, or it hasn't been updated in `$DAYS` days |
| Slack mention or DM | replies in its thread (or later in the DM), reacts to it with :white_check_mark:, or reviews the PR it asks about | it is older than `$DAYS` days, or the PR it asks about is closed |
| Linear | reads the notification | — |

Tell jml about the :white_check_mark: reaction whenever a Slack item looks already handled somewhere else, such as on a call or in another thread.

## Steps

Run steps 1–3 in parallel.

1. **GitHub.** Run `python3 ~/.claude/skills/waiting-on-me/scripts/waiting.py --days $DAYS`. It prints `items`, oldest wait first, each with a `kind`:
   - `asked`: a person @-mentioned jml after jml last commented or reviewed. `asks[]` holds the last two asks with their comment `url` and an `excerpt`.
   - `review`: jml is a requested reviewer by name. A team request doesn't count: jml can't tell from the request alone whether jml is the one expected to review.
   - `bot-review`: a bot's PR requests jml by name. Count these but don't list them.

   `stale: true` means nobody has touched the PR or issue in 30 days. Read the full JSON. For an `asked` item, use the excerpt to say in a few words what the person needs, such as "re-approve after a rebase" or "approve the tf-plan gate".

2. **Slack.** Use `slack_search_public_and_private` with `sort: timestamp`, `include_context: false` and `response_format: detailed`, because only the detailed format carries `Permalink`, `Message_ts` and the channel ID. Fetch up to three pages of each search:
   - Mentions: keywords `["<@U08S0H4L5KJ>"]`, filters `after:$SINCE -from:<@U08S0H4L5KJ>`.
   - DMs and group DMs: filters `after:$SINCE -from:<@U08S0H4L5KJ>`, `channel_types: im,mpim`.
   - Already handled: filters `hasmy::white_check_mark: after:$SINCE`. Drop every candidate whose `Message_ts` appears here.

   Keep a message only if it asks jml for something: a question, a review, a restamp, a decision, or help. Drop thanks, FYIs, announcements that credit jml, and a bare "cc" with no question. In a DM, the ask is the latest message from the other person, not every message in the conversation.

   For each message you keep, check whether jml already answered:
   - **In Slack.** If the permalink has a `thread_ts`, or `Reply count` is above zero, call `slack_read_thread` with `response_format: concise`, the channel ID, and the thread's ts (`thread_ts` when present, `Message_ts` otherwise). Don't pass `oldest`: it can drop the ask itself. The replies come back oldest first, so drop the ask if a message from jml follows it. A DM message outside a thread needs `slack_read_channel` with `limit: 20` instead.
   - **On GitHub.** When the ask is about a PR (a review, restamp, approval or merge) and links it or names it in the thread, run `gh pr view <N> --repo <owner/repo> --json state,latestReviews`. Drop the ask if the PR is closed or merged, or if jml reviewed it after the ask.

   Read at most 15 threads, newest asks first, and say how many you didn't check.

3. **Linear.** Call go-linear `notification_list` with `limit: 50`. The output is large, so read it from the saved file with `jq -r '.stdout' <file> | jq '.nodes[]'`. Keep unread notifications (`readAt` null) whose `type` contains `Mention` or `Assigned`. Status changes, new comments and subscriptions are FYI, so leave them out. Each issue's link is `https://linear.app/chainguard/issue/<identifier>`.

4. **Report** as your final message, which is all jml sees of this run, oldest wait first within each group. Every item starts with a link. Leave out a group that has no items.

   ```
   **Blocking someone (GitHub)**
   - [infra#5422](<ask url>) shimmerjs, since 19 Nov: re-approve the rebased head (last-push rule).

   **Review requested by name**
   - [mono#82041](<pr url>) javacruft, since 7 Nov: guarded-os shared-library bot design.

   **Slack**
   - [#team-channel, A. Colleague, 18 Nov](<permalink>): is a manual step needed to apply the stereo settings?

   **Linear**
   - [DEV-3663](<url>) Markus mentioned you: `pull_request_target` deadline 2 Dec.
   ```

   - Link each GitHub ask to the comment (`asks[-1].url`), and fall back to the PR's URL.
   - Link each Slack item to its permalink. Never list one without a link.
   - After the groups, add one line of counts: how many stale items you moved to the end, how many bot PRs, and how many Slack threads you didn't check.
   - Close with the one item that unblocks the most for the least effort, such as a re-approval or a gate approval, and say why.
