---
name: pr-map
description: Map jml's open PRs as a dependency DAG with two lists, what waits on review and what waits on jml, publish it as one Artifact that stays at the same link, keep it current as PRs change, and ask jml the questions that unblock his PRs. Use when jml says "/pr-map", "make a DAG of my PRs", "what's waiting on review and what's waiting on me", "where am I blocked", or has lost track of his open PRs.
---

# PR map

`scripts/pr_map.py` fetches jml's open PRs, works out who owes each one its next move, and writes an HTML page: a Mermaid DAG of the dependencies, then **Waiting on me**, **Waiting on review** and **Waiting on something else**. It builds on the rollout skill: it reads `rollout.py sweep`, `rollout.py status` for each rollout, `chase`, and `~/.local/state/rollouts/requests.jsonl`.

State is in `~/.local/state/pr-map/`:

| File | What it holds |
|---|---|
| `edges.json` | Dependencies GitHub doesn't record, and per-PR overrides (see below) |
| `pr-map.html` | The page |
| `pr-map.json` | The last classification, which the next render diffs against |
| `pr-map.fp` | Fingerprints the watcher compares against |
| `artifact-url` | The Artifact the page is published to |

## Steps

1. **Render.** Run `python3 ~/.claude/skills/pr-map/scripts/pr_map.py render`. It prints the counts per list and `changes` since the last render.
2. **Add dependencies** for any PR in `changes` with `"from": "new"`. Read its body for "stacked on", "depends on", "merge … first", "after #N deploys", "Design-Doc:", and PRs it cites. Add each as an edge to `edges.json` and render again:
   - `stack`: built on another PR's branch or commits, or merges it in.
   - `deploy`: lands only after another PR reaches staging or prod.
   - `design`: its design doc is another PR.
   - `soft`: cites it or would conflict with it. A soft edge never holds review back.

   Base branches and rollout manifests already give the edges for PRs in rollouts; don't copy those.
3. **Publish.** Copy `pr-map.html` into the scratchpad directory and publish that copy with the Artifact tool. If `artifact-url` exists, first `read` that URL, then publish with `url` set to it so the link stays the same. On a first publish, write the URL the tool returns to `artifact-url`.
4. **Unblock.** For each PR in **Waiting on me**, first check whether another session already moved it: commits or replies after the finding, or a session working on its branch. Then ask jml the rest in one `AskUserQuestion` call: at most four questions, each naming its PR with its title. Typical questions: mark these drafts ready, fix this failing check here, chase these overdue reviews, merge this approved PR. Do what he picks, then render and publish again.
5. **Keep it current.** Start `python3 ~/.claude/skills/pr-map/scripts/pr_map.py watch` with `run_in_background: true`. It checks every 10 minutes, and checks rollout deploy phases every third check, because those calls cost the most GraphQL points. When it exits, a PR's status has changed: render, report the `changes` in a line or two, publish, ask about anything new in **Waiting on me**, and start the watcher again. Run one watcher at a time. Render again after your own changes too (marking ready, merging, moving to draft); the watcher would only catch them on its next check.

## How a PR is classified

jml's rules, given 2026-11-24, in the order they apply:

- **Failing or pending CI is never "waiting on review".** Failing CI is jml's move. Pending CI waits on the checks. Checks that clear only on a person's approval (`doc-approval`, `cg-codeowners-check`, `Checks review gate`) count as review, not CI.
- **A PR stacked on an unmerged PR, or waiting for one to deploy, is never "waiting on review".** It is rebased once that lands, so a review now is wasted. It belongs in draft: a PR in review shows under **Waiting on me** as "move back to draft" (the rollout skill's `mark-draft` action does it under jml's standing yes), and a draft waits on its dependency.
- A PR whose only failure is `pr-hygiene`, because its design doc PR hasn't merged, waits on that design doc.
- Everything else follows the rollout skill's `/eod` buckets: replies and fixes are jml's, requests sent or overdue are review, held, owned-by-a-session and running PRs are something else.

When the rules get a PR wrong, add an override to `edges.json`: `"overrides": {"<owner/repo#N>": {"owner": "me|review|other", "why": "<the fact that decides it>"}}`. Delete it once the PR's state changes, because an override is never re-checked.

## Standing yeses

- Mark a green draft that no dependency holds back ready for review, and send its review request to the channel the rollout config maps (`:pr: [<title>](<url>)`, nothing else). Search Slack for a hand-sent request first and record it with `rollout.py record-request` if there is one.
- Move a PR blocked on an unlanded PR back to draft (`gh pr ready --undo`).
- Ask before merging anything. For `chainguard-sandbox/sandbox-of-power`, jml merges without review: offer to squash-merge a green PR there instead of requesting a review.

## When GitHub rate-limits

All of this shares jml's GraphQL budget with rollout loops and other sessions. If `render` fails with "API rate limit already exceeded", stop the watcher, retry `render` every two minutes in the background until it succeeds, then publish and restart the watcher. `gh api graphql -f query='{ rateLimit { remaining resetAt } }'` gives the reset time.
