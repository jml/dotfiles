"""Pure rollout logic: no I/O. Inputs are plain dicts; outputs are plain dicts.

A rollout is a DAG of nodes. A node is a pull request (`pr`) or a step a
person or a session performs (`step`). Each node lists the conditions it waits
for in `after`, written `<node>` (the node's default condition) or
`<node>@<condition>`:

  pr:   merged (default), deployed:<env>
  step: done (default)

A node may also be `work`: a change not yet opened as a PR, being built in a
session (`session`) or by someone; it gains `pr` once the PR exists. A node
with `proposed` is new work found during the rollout that jml hasn't yet
accepted into it: it blocks whatever waits on it until jml decides.

`facts` holds what the I/O layer fetched: `prs` keyed by node id, `deploys`
keyed by node id then env (True, False or None when it can't tell), and
`checks` keyed by step node id (True when the step's check passed).
"""

from __future__ import annotations

import re
from datetime import datetime, timedelta

# Seconds until the next tick, by the cheapest thing still in motion.
TICK_CHECKS_RUNNING = 300
TICK_DEPLOYING = 900
TICK_WAITING_ON_PEOPLE = 1800
TICK_IDLE = 3600


def parse_after(entry: str, nodes: dict) -> tuple[str, str]:
    node_id, _, cond = entry.partition("@")
    if node_id not in nodes:
        raise ValueError(f"unknown node {node_id!r} in after: {entry!r}")
    if not cond:
        cond = "done" if "step" in nodes[node_id] else "merged"
    return node_id, cond


def validate(manifest: dict) -> list[str]:
    errors = []
    nodes = {n["id"]: n for n in manifest.get("nodes", [])}
    if len(nodes) != len(manifest.get("nodes", [])):
        errors.append("duplicate node ids")
    for n in manifest.get("nodes", []):
        if (("pr" in n) or ("work" in n)) == ("step" in n):
            errors.append(f"{n['id']}: exactly one of pr (or work) or step")
        for a in n.get("after", []):
            try:
                _, cond = parse_after(a, nodes)
            except ValueError as e:
                errors.append(f"{n['id']}: {e}")
                continue
            if cond not in ("merged", "done") and not cond.startswith("deployed:"):
                errors.append(f"{n['id']}: unknown condition {cond!r}")
    if not errors and _has_cycle(nodes):
        errors.append("after edges form a cycle")
    return errors


def _has_cycle(nodes: dict) -> bool:
    state: dict[str, int] = {}

    def visit(i: str) -> bool:
        if state.get(i) == 1:
            return True
        if state.get(i) == 2:
            return False
        state[i] = 1
        for a in nodes[i].get("after", []):
            if visit(parse_after(a, nodes)[0]):
                return True
        state[i] = 2
        return False

    return any(visit(i) for i in nodes)


def satisfied(node_id: str, cond: str, nodes: dict, facts: dict, marks: dict):
    """True, False, or None when the facts can't say."""
    node = nodes[node_id]
    if node.get("proposed") or ("work" in node and "pr" not in node):
        return False
    if "step" in node:
        return bool(marks.get(node_id)) or bool(facts.get("checks", {}).get(node_id))
    pr = facts["prs"].get(node_id) or {}
    if cond == "merged":
        return pr.get("state") == "MERGED"
    if cond.startswith("deployed:"):
        if pr.get("state") != "MERGED":
            return False
        return facts.get("deploys", {}).get(node_id, {}).get(cond.split(":", 1)[1])
    raise ValueError(cond)


def _checks(pr: dict, review_gated: set[str]) -> tuple[str, list[str], list[str]]:
    """Summarise the head commit's checks as (state, failing names, awaiting-review names).

    A review-gated check resolves only when someone approves the PR, so it is
    left out of the state: counted as running, it would hold the rollout to the
    CI cadence for days, and counted as failing, it would send the session to fix it.
    """
    failing, awaiting, pending = [], [], False
    for c in pr.get("checks", []):
        result = (c.get("conclusion") or c.get("state") or "").upper()
        status = (c.get("status") or "").upper()
        if c["name"] in review_gated:
            if result not in ("SUCCESS", "NEUTRAL", "SKIPPED"):
                awaiting.append(c["name"])
        elif result in ("FAILURE", "ERROR", "TIMED_OUT", "CANCELLED", "ACTION_REQUIRED", "STARTUP_FAILURE"):
            failing.append(c["name"])
        elif status in ("QUEUED", "IN_PROGRESS", "PENDING", "WAITING", "REQUESTED") or result in ("PENDING", "EXPECTED"):
            pending = True
    awaiting = sorted(set(awaiting))
    if failing:
        return "failing", sorted(failing), awaiting
    return ("pending" if pending else "green"), [], awaiting


def _awaiting_author(pr: dict) -> dict:
    """Unresolved threads whose last comment isn't the PR author's, split by who started them."""
    open_ = [t for t in pr.get("threads", []) if not t["resolved"] and t["last_author"] != pr["author"]]
    bots = sum(1 for t in open_ if t.get("started_by_bot"))
    return {"bot_threads": bots, "person_threads": len(open_) - bots}


# Check runs that carry a review bot's verdict. In repos where Checks reviews,
# `reviewer/<team>` is one reviewer's verdict (success = approve) and the gate
# succeeds when Checks has approved, or is about to, for every changed path.
BOT_REVIEW_CHECKS = ("reviewer/", "Checks review gate", "claude-code-action", "claude-review")
BOT_APPROVAL_GATE = "Checks review gate"


def bot_review(pr: dict, config: dict) -> dict:
    """Where the review bots are on this PR's head.

    state: none (no bot reviews here), pending (a bot review is running),
    findings (a bot's thread is unresolved), approved (the gate passed; its
    approving review follows), done (the bots finished without approving, so
    a person must review).
    """
    prefixes = tuple(config.get("bot_review_checks", BOT_REVIEW_CHECKS))
    gate = config.get("bot_approval_gate", BOT_APPROVAL_GATE)
    ran = [
        c for c in pr.get("checks", [])
        if c["name"].startswith(prefixes) and (c.get("conclusion") or "").upper() != "SKIPPED"
    ]
    pending = sorted({
        c["name"] for c in ran
        if (c.get("status") or "").upper() in ("QUEUED", "IN_PROGRESS", "PENDING", "WAITING", "REQUESTED")
    })
    open_threads = _awaiting_author(pr)["bot_threads"]
    present = ran or any(t.get("started_by_bot") for t in pr.get("threads", []))
    if not present:
        state = "none"
    elif pending:
        state = "pending"
    elif open_threads:
        state = "findings"
    elif any(c["name"] == gate and (c.get("conclusion") or "").upper() == "SUCCESS" for c in ran):
        state = "approved"
    else:
        state = "done"
    return {"state": state, "pending": pending, "open_threads": open_threads}


def resolve_channels(manifest: dict, pr: dict, config: dict) -> list[dict]:
    """Candidate Slack channels for a review request, most specific first."""
    out: list[dict] = []

    def add(channel: str, why: str):
        if channel and channel not in [c["channel"] for c in out]:
            out.append({"channel": channel, "why": why})

    if manifest.get("channel"):
        add(manifest["channel"], "rollout channel")
    for rule in config.get("channels", {}).get("paths", []):
        if rule.get("repo") and rule["repo"] != pr["repo"]:
            continue
        if any(f.startswith(rule["prefix"]) for f in pr.get("files", [])):
            add(rule["channel"], f"path {rule['prefix']}")
    teams = config.get("channels", {}).get("teams", {})
    for t in pr.get("requested_teams", []):
        if t in teams:
            add(teams[t], f"team {t}")
    if pr["repo"] in config.get("channels", {}).get("repos", {}):
        add(config["channels"]["repos"][pr["repo"]], f"repo {pr['repo']}")
    return out


def _latest_request(requests: list[dict], ref: str) -> dict | None:
    mine = [r for r in requests if r["pr"] == ref]
    return max(mine, key=lambda r: r["sent_at"]) if mine else None


def evaluate(manifest: dict, facts: dict, marks: dict, requests: list[dict], config: dict) -> dict:
    nodes = {n["id"]: n for n in manifest["nodes"]}
    consumers: dict[str, list[str]] = {}
    for n in manifest["nodes"]:
        for a in n.get("after", []):
            dep, cond = parse_after(a, nodes)
            consumers.setdefault(dep, []).append(cond)

    out_nodes, ticks = [], [TICK_IDLE]
    for n in manifest["nodes"]:
        waiting = []
        for a in n.get("after", []):
            dep, cond = parse_after(a, nodes)
            s = satisfied(dep, cond, nodes, facts, marks)
            if s is not True:
                waiting.append({"node": dep, "condition": cond, "unknown": s is None})
        deps_met = not waiting
        actions: list[dict] = []
        entry = {"id": n["id"], "waiting_on": waiting}

        if n.get("proposed"):
            pr = facts["prs"].get(n["id"]) if "pr" in n else None
            entry |= {
                "kind": "pr" if "pr" in n else ("step" if "step" in n else "work"),
                "title": (pr or {}).get("title") or n.get("work") or n.get("step"),
                "phase": "proposed",
                "actions": [{"kind": "decide-scope", "auto": False, "detail": n["proposed"]}],
            }
            if pr:
                entry |= {"ref": n["pr"], "url": pr["url"]}
            ticks.append(TICK_WAITING_ON_PEOPLE)
            out_nodes.append(entry)
            continue

        if "work" in n and "pr" not in n:
            entry |= {"kind": "work", "title": n["work"], "phase": "building", "actions": []}
            if n.get("session"):
                entry["session"] = n["session"]
            ticks.append(TICK_WAITING_ON_PEOPLE)
            out_nodes.append(entry)
            continue

        if "step" in n:
            entry |= {"kind": "step", "title": n["step"]}
            if satisfied(n["id"], "done", nodes, facts, marks):
                entry["phase"] = "done"
            elif not deps_met:
                entry["phase"] = "blocked"
            elif n.get("by") == "automatic":
                # Something else does it (a schedule, a bot); its check reports when.
                entry["phase"] = "waiting"
                ticks.append(TICK_DEPLOYING)
            else:
                entry["phase"] = "actionable"
                actions.append({"kind": "do-step", "auto": n.get("by") == "session", "detail": n.get("by", "jml")})
                ticks.append(TICK_WAITING_ON_PEOPLE)
            entry["actions"] = actions
            out_nodes.append(entry)
            continue

        pr = facts["prs"][n["id"]]
        entry |= {"kind": "pr", "ref": n["pr"], "title": pr["title"], "url": pr["url"]}
        if pr["state"] == "CLOSED":
            entry["phase"] = "closed"
            actions.append({"kind": "decide", "auto": False, "detail": "closed without merging; drop the node or replace it"})
        elif pr["state"] == "MERGED":
            pending = [
                c for c in consumers.get(n["id"], [])
                if c.startswith("deployed:") and satisfied(n["id"], c, nodes, facts, marks) is not True
            ]
            entry["phase"] = "deploying" if pending else "done"
            if pending:
                entry["deploying_to"] = sorted({c.split(":", 1)[1] for c in pending})
                ticks.append(TICK_DEPLOYING)
        else:
            checks, failing, awaiting = _checks(pr, set(config.get("review_gated_checks", [])))
            entry |= {"checks": checks, "failing": failing, "review": pr.get("review_decision")}
            if awaiting:
                entry["awaiting_review"] = awaiting
            if config.get("me") and pr["author"] != config["me"]:
                # Someone else's PR: its author rebases, fixes, answers, requests
                # review and merges it, so the rollout only waits for it.
                entry["author"] = pr["author"]
                entry["phase"] = "waiting" if deps_met else "blocked"
                entry["actions"] = actions
                ticks.append(TICK_WAITING_ON_PEOPLE)
                out_nodes.append(entry)
                continue
            if checks == "pending":
                ticks.append(TICK_CHECKS_RUNNING)
            if pr.get("mergeable") == "CONFLICTING":
                # Only the next leg is pushed after a rebase; later legs would
                # just be rebased and pushed again, re-running their CI.
                actions.append({"kind": "rebase", "auto": deps_met, "detail": "push now" if deps_met else "rebase locally, push when next"})
            if failing:
                if deps_met:
                    actions.append({"kind": "fix-ci", "auto": True, "detail": failing})
                else:
                    actions.append({"kind": "check-failures", "auto": False, "detail": {"failing": failing, "note": "dependencies not landed; failures may be expected"}})
            threads = _awaiting_author(pr)
            if threads["bot_threads"] or threads["person_threads"] or pr.get("review_decision") == "CHANGES_REQUESTED":
                actions.append({"kind": "respond", "auto": True, "detail": threads})
            bots = bot_review(pr, config)
            if bots["state"] != "none":
                entry["bot_review"] = bots["state"]
            if pr["draft"]:
                if checks == "green" and not n.get("hold_draft"):
                    # jml's standing yes: a green draft goes ready without asking;
                    # `hold_draft` keeps one that is a draft on purpose.
                    actions.append({"kind": "mark-ready", "auto": True, "detail": "draft is green"})
            elif pr.get("review_decision") == "APPROVED":
                if awaiting:
                    # Approval has landed; the review-gated checks re-run on it.
                    ticks.append(TICK_CHECKS_RUNNING)
                elif deps_met and checks == "green" and pr.get("mergeable") == "MERGEABLE":
                    actions.append({"kind": "merge", "auto": False, "detail": "approved, green, dependencies landed"})
            # Wait for a run in progress: a review request offered while checks are
            # pending would vanish if they then fail, and come back on the next push.
            # People hear about a PR only once the review bots are finished with
            # it: their findings addressed, and no approval of theirs on the way.
            elif checks == "green" and bots["state"] in ("pending", "approved"):
                ticks.append(TICK_CHECKS_RUNNING)
            elif checks == "green" and bots["state"] == "findings":
                pass  # the respond action above addresses them
            elif checks == "green":
                req = _latest_request(requests, n["pr"])
                if req is None:
                    channels = resolve_channels(manifest, pr, config)
                    # jml's standing yes covers the templated request to a known
                    # channel; with no channel mapped, jml has to name one.
                    actions.append({"kind": "request-review", "auto": bool(channels), "detail": {"channels": channels, "message": f":pr: [{pr['title']}]({pr['url']})"}})
                ticks.append(TICK_WAITING_ON_PEOPLE)
            entry["phase"] = "actionable" if actions else ("blocked" if not deps_met else "waiting")
        entry["actions"] = actions
        out_nodes.append(entry)

    return {
        "name": manifest["name"],
        "nodes": out_nodes,
        "complete": all(e["phase"] == "done" for e in out_nodes),
        "next_tick_seconds": min(ticks),
    }


def ci_settled(pr: dict, config: dict) -> bool:
    """True once nothing on the PR's head is still running that a tick would act
    on: CI has finished (checks that clear only on approval don't count), and so
    have the review bots."""
    checks, _, _ = _checks(pr, set(config.get("review_gated_checks", [])))
    return pr["state"] != "OPEN" or (checks != "pending" and bot_review(pr, config)["state"] != "pending")


def whose_move(action: dict) -> str:
    """Who acts next on an action: the session, jml, or someone else jml may chase."""
    if action["auto"]:
        return "session"
    if action["kind"] == "do-step" and action["detail"] == "external":
        return "other"
    return "jml"


def weekday_hours(start: datetime, end: datetime) -> float:
    """Hours between start and end that fall Monday to Friday (UTC), day and night.

    Reviewers span time zones, so no working-day window is assumed.
    """
    total, t = 0.0, start
    while t < end:
        step = min(end, (t + timedelta(hours=1)).replace(minute=0, second=0, microsecond=0))
        if t.weekday() < 5:
            total += (step - t).total_seconds() / 3600
        t = step
    return total


def chase_threshold_hours(pr: dict, config: dict) -> float:
    c = config.get("chase", {})
    size = pr.get("additions", 0) + pr.get("deletions", 0)
    return min(c.get("max_hours", 16), c.get("base_hours", 4) + size / c.get("lines_per_hour", 250))


def chase(requests: list[dict], prs: dict, now: datetime, config: dict) -> list[dict]:
    """Review requests that have gone unanswered past their threshold.

    `prs` is keyed by "owner/repo#N". A request counts as answered once the PR
    is merged, closed or approved, or once a person other than the author
    reviewed or commented on it after the latest request went out. The clock
    runs from the latest request for the PR, whatever channel it went to.
    """
    ignored = set(config.get("chase", {}).get("ignore_authors", []))
    by_pr: dict[str, list[dict]] = {}
    for r in requests:
        by_pr.setdefault(r["pr"], []).append(r)
    overdue = []
    for ref, rs in sorted(by_pr.items()):
        r = max(rs, key=lambda x: x["sent_at"])
        pr = prs.get(ref)
        if pr is None or pr["state"] != "OPEN" or pr.get("review_decision") == "APPROVED":
            continue
        sent = datetime.fromisoformat(r["sent_at"])
        answered = any(
            a["author"] != pr["author"]
            and not a.get("bot")
            and a["author"] not in ignored
            and not a["author"].endswith("[bot]")
            and datetime.fromisoformat(a["at"]) > sent
            for a in pr.get("activity", [])
        )
        if answered:
            continue
        waited = weekday_hours(sent, now)
        limit = chase_threshold_hours(pr, config)
        if waited >= limit:
            overdue.append({
                "pr": ref,
                "title": pr["title"],
                "url": pr["url"],
                "rollout": r["rollout"],
                "channel": r["channel"],
                "channels_tried": sorted({x["channel"] for x in rs}),
                "permalink": r.get("permalink"),
                "sent_at": r["sent_at"],
                "weekday_hours_waited": round(waited, 1),
                "threshold_hours": round(limit, 1),
                "requested_teams": pr.get("requested_teams", []),
                "requested_users": pr.get("requested_users", []),
            })
    return sorted(overdue, key=lambda o: -o["weekday_hours_waited"])


# End-of-day sweep buckets, in the order a PR is tested against them: a PR
# lands in the first that fits.
EOD_BUCKETS = ("held", "reply", "fix", "merge", "owned", "ask", "blocked", "ready", "bots", "overdue", "waiting", "running", "parked")


def _parse(ts: str | None) -> datetime | None:
    return datetime.fromisoformat(ts.replace("Z", "+00:00")) if ts else None


def last_activity(pr: dict) -> datetime | None:
    """The latest commit, or comment or review by a person, on the PR."""
    times = [_parse(pr.get("last_commit_at"))]
    times += [_parse(a["at"]) for a in pr.get("activity", []) if not a.get("bot") and not a["author"].endswith("[bot]")]
    times = [t for t in times if t]
    return max(times) if times else None


def unchecked_todos(body: str) -> list[str]:
    """Unticked boxes under a "To-do before merge" heading in a PR description."""
    out, inside = [], False
    for line in body.splitlines():
        if line.lstrip().startswith("#"):
            inside = "to-do before merg" in line.lower() or "todo before merg" in line.lower()
            continue
        item = line.strip()
        if inside and item.startswith(("- [ ]", "* [ ]")):
            out.append(item[5:].strip())
    return out


def builds_on(ref: str, prs: dict) -> list[str]:
    """Other open PRs this one builds on: its base is their branch, or it carries all their commits.

    Commits match by oid, or by headline once a rebase has rewritten the oid. A
    PR carrying every commit of another, plus more, sits on top of it; two
    siblings off one parent each have a commit the other lacks, so neither
    counts as building on the other.
    """
    me = prs[ref]
    oids = {c["oid"] for c in me.get("commits", [])}
    lines = {c["headline"] for c in me.get("commits", [])}
    out = []
    for other_ref, other in sorted(prs.items()):
        if other_ref == ref or other["repo"] != me["repo"] or other["state"] != "OPEN":
            continue
        theirs = other.get("commits", [])
        if me.get("base") and me.get("base") == other.get("head"):
            out.append(other_ref)
        elif theirs and len(theirs) < len(me.get("commits", [])) and all(
            c["oid"] in oids or c["headline"] in lines for c in theirs
        ):
            out.append(other_ref)
    return out


def session_on(ref: str, prs: dict, sessions: list[dict]) -> str | None:
    """The live session working on this PR: one whose folder has the PR's branch
    checked out, or whose title names "#N" when no other open PR has number N."""
    pr = prs[ref]
    for s in sessions:
        if s.get("branch") and s["branch"] == pr.get("head"):
            return s["name"]
    if sum(p["number"] == pr["number"] for p in prs.values()) == 1:
        for s in sessions:
            if f"#{pr['number']}" in s["name"]:
                return s["name"]
    return None


def eod(prs: dict, requests: list[dict], holds: dict, now: datetime, first_run: datetime, config: dict,
        sessions: list[dict] | None = None) -> dict:
    """Sort open PRs that belong to no rollout into end-of-day buckets.

    `prs` is keyed by "owner/repo#N". Each PR is evaluated as its own one-node
    rollout, so the rules that decide mark-ready and request-review are the
    ones /rollout uses. `holds` maps a ref to {"until": ISO date, "reason"}.

    mark-ready is automatic only for drafts opened since `first_run`, the first
    /eod: a draft older than that may be a draft on purpose, so jml is asked
    once, and holding it records the answer.

    A PR ready for review whose live session is in `sessions` goes to `owned`
    and gets no request: that session sends it.

    A green draft is never offered for mark-ready (bucket `blocked`) while a
    live session in `sessions` works on it, while it builds on another open PR,
    while its base isn't the default branch, or while its description has an
    unticked "To-do before merge" item: jml wants only drafts that could merge
    now.
    """
    manifest = {"name": "eod", "nodes": [{"id": ref, "pr": ref} for ref in sorted(prs)]}
    report = evaluate(manifest, {"prs": prs, "deploys": {}, "checks": {}}, {}, requests, config)
    overdue = {o["pr"]: o for o in chase(requests, prs, now, config)}
    stale_after = timedelta(days=config.get("eod", {}).get("stale_draft_days", 7))
    gated = set(config.get("review_gated_checks", []))

    out = []
    for entry in report["nodes"]:
        ref = entry["ref"]
        pr = prs[ref]
        actions = {a["kind"]: a for a in entry["actions"]}
        threads = _awaiting_author(pr)
        checks, failing, _ = _checks(pr, gated)
        hold = holds.get(ref)
        active = last_activity(pr)
        created = _parse(pr.get("created_at"))
        if "mark-ready" in actions:
            actions["mark-ready"] = actions["mark-ready"] | {"auto": bool(created and created >= first_run)}

        if hold and hold["until"] >= now.date().isoformat():
            bucket, detail = "held", hold
        elif threads["person_threads"] or pr.get("review_decision") == "CHANGES_REQUESTED":
            bucket, detail = "reply", {"person_threads": threads["person_threads"], "review": pr.get("review_decision")}
        elif failing or pr.get("mergeable") == "CONFLICTING" or entry.get("bot_review") == "findings":
            bucket, detail = "fix", {"failing": sorted(set(failing)), "conflicting": pr.get("mergeable") == "CONFLICTING",
                                     "bot_findings": threads["bot_threads"]}
        elif "merge" in actions:
            bucket, detail = "merge", None
        elif "request-review" in actions and (owner := session_on(ref, prs, sessions or [])):
            # The live session working on the PR sends its own review request.
            bucket, detail = "owned", {"session": owner}
        elif "request-review" in actions:
            bucket, detail = "ask", actions["request-review"]["detail"]
        elif "mark-ready" in actions and (blockers := {k: v for k, v in {
            "session": session_on(ref, prs, sessions or []),
            "builds_on": builds_on(ref, prs),
            "base": pr.get("base") if pr.get("base") and pr.get("default_branch") and pr["base"] != pr["default_branch"] else None,
            "todos": unchecked_todos(pr.get("body", "")),
        }.items() if v}):
            bucket, detail = "blocked", blockers
        elif "mark-ready" in actions:
            bucket, detail = "ready", None
        elif checks == "green" and entry.get("bot_review") in ("pending", "approved"):
            bucket, detail = "bots", entry["bot_review"]
        elif ref in overdue:
            bucket, detail = "overdue", overdue[ref]
        elif not pr["draft"] and _latest_request(requests, ref):
            r = _latest_request(requests, ref)
            bucket, detail = "waiting", {"channel": r["channel"], "sent_at": r["sent_at"], "teams": pr.get("requested_teams", [])}
        elif checks == "pending":
            bucket, detail = "running", None
        else:
            bucket, detail = "parked", None

        keep = {"held": [], "reply": [], "fix": [], "merge": ["merge"], "ask": ["request-review"], "ready": ["mark-ready"]}
        out.append({
            "ref": ref,
            "title": pr["title"],
            "url": pr["url"],
            "draft": pr["draft"],
            "bucket": bucket,
            "detail": detail,
            "stale": bool(pr["draft"] and bucket != "held" and active and now - active > stale_after),
            "last_activity": active.isoformat() if active else None,
            "actions": [actions[k] for k in keep.get(bucket, []) if k in actions],
        })

    ask_ages = [now - t for t in (_parse(prs[e["ref"]].get("last_commit_at")) for e in out if e["bucket"] == "ask") if t]
    return {
        "prs": out,
        "counts": {b: sum(e["bucket"] == b for e in out) for b in EOD_BUCKETS},
        "oldest_ask_hours": round(max(ask_ages).total_seconds() / 3600, 1) if ask_ages else None,
    }


# Standup: blockers and cross-team dependencies, compared with the last standup.

# Words shaped like a Linear identifier that aren't one.
NOT_ISSUE_PREFIXES = {"UTF", "SHA", "ISO", "RFC", "CVE", "GHSA", "PEP", "TLS", "HTTP", "AES"}
_ISSUE_ID = re.compile(r"\b([A-Z][A-Z0-9]{1,9}-\d+)\b")


def issue_ids(text: str) -> set[str]:
    return {m for m in _ISSUE_ID.findall(text or "") if m.split("-")[0] not in NOT_ISSUE_PREFIXES}


_TRACKING_LINE = re.compile(r"^\s*(Tracking|Fixes|Closes|Resolves|Part of)\b", re.IGNORECASE)


def pr_issue_ids(pr: dict) -> set[str]:
    """Linear issues a PR works on: named in its title or branch, or on a
    `Tracking:`-style line of its description. Other mentions in the
    description are context (jml cites related issues as reference links), so
    they don't count."""
    tracking = "\n".join(line for line in (pr.get("body") or "").splitlines() if _TRACKING_LINE.match(line))
    return issue_ids(pr.get("title", "")) | issue_ids((pr.get("head") or "").upper()) | issue_ids(tracking)


def pr_mentions_issue(pr: dict) -> bool:
    """Whether a PR names any Linear issue anywhere, so belongs to planned work."""
    return bool(pr_issue_ids(pr) | issue_ids(pr.get("body", "")))


def person_kind(who: str | None, sc: dict) -> str:
    """subteam, pod or other, for a Linear display name or a GitHub login."""
    def names(group):
        return {v for p in sc.get(group, []) for v in (p.get("name"), p.get("github")) if v}
    if who in names("subteam"):
        return "subteam"
    if who in names("pod"):
        return "pod"
    return "other"


def _issue_counterpart(issue: dict, sc: dict) -> dict:
    team = issue["id"].split("-")[0]
    if issue.get("assignee"):
        return {"kind": person_kind(issue["assignee"], sc), "label": f"{issue['assignee']} ({team})"}
    kind = "subteam" if team in sc.get("subteam_linear_teams", []) else "other"
    return {"kind": kind, "label": f"{team}, unassigned"}


def _review_counterparts(entry: dict, pr: dict, config: dict) -> list[dict]:
    """Who a waiting review is waiting on: requested teams and people, or the team
    whose channel the request went to. Home teams count as the subteam."""
    sc = config.get("standup", {})
    home = set(sc.get("home_github_teams", []))
    by_channel = {}
    for team, channel in config.get("channels", {}).get("teams", {}).items():
        by_channel.setdefault(channel, team)
    teams = set(pr.get("requested_teams", []))
    channel = (entry.get("detail") or {}).get("channel")
    if not teams and channel:
        teams.add(by_channel.get(channel, channel))
    out = [{"kind": "subteam" if t in home else "other", "label": t} for t in sorted(teams)]
    out += [{"kind": person_kind(u, sc), "label": u} for u in sorted(pr.get("requested_users", []))]
    return [c for c in out if c["kind"] != "subteam"]


WHOSE_MOVE = {
    "held": "jml", "reply": "jml", "fix": "jml", "merge": "jml", "owned": "session", "ask": "jml",
    "blocked": "jml", "ready": "jml", "bots": "bots", "overdue": "reviewers", "waiting": "reviewers",
    "running": "ci", "parked": "jml",
}
OPEN_STATUS_TYPES = ("unstarted", "started")
CLOSED_STATUS_TYPES = ("completed", "canceled")


def _diff(now_items: dict, prev_items: dict, track_state: bool) -> dict:
    out = {"new": [], "standing": [], "gone": []}
    if track_state:
        out["changed"] = []
    for key, item in sorted(now_items.items()):
        before = prev_items.get(key)
        if before is None:
            out["new"].append(item)
        elif track_state and before.get("state") != item.get("state"):
            out["changed"].append(item | {"was": before.get("state")})
        else:
            out["standing"].append(item)
    out["gone"] = [prev_items[k] for k in sorted(set(prev_items) - set(now_items))]
    return out


def standup(sweep: dict, prs: dict, rollouts: dict, linear: dict, merged: list[dict], inbound: list[dict],
            previous: dict | None, config: dict) -> dict:
    """Blockers, dependencies, Linear drift and unplanned fixes for jml's standup.

    `sweep` is core.eod over every open PR of jml's, `prs` their facts and
    `rollouts` maps the ones in a rollout to its name: their review requests
    and mark-ready belong to the rollout's session, so they carry no actions.
    `linear["issues"]` holds jml's open issues and every issue they block or are
    blocked by, each with id, title, url, status, status_type, assignee and,
    for jml's, blocked_by and blocks. `merged` is jml's PRs merged since the
    last standup and `inbound` the open PRs whose author asked jml directly
    for review. `previous` is the last standup's snapshot, or None for the
    first run, which is a baseline: nothing in it counts as new.

    A dependency is anything between jml and someone outside the subteam:
    the pod's other members, or another team.
    """
    sc = config.get("standup", {})
    me = sc.get("linear_name")
    issues = {i["id"]: i for i in linear.get("issues", [])}
    mine = {k: i for k, i in issues.items() if i.get("assignee") == me and i.get("status_type") in OPEN_STATUS_TYPES}

    def is_open(issue_id: str) -> bool:
        # An issue the facts don't describe is assumed open: better a stale
        # dependency reported than a live one dropped.
        return issues.get(issue_id, {}).get("status_type", "started") not in CLOSED_STATUS_TYPES

    def other(issue_id: str) -> dict:
        i = issues.get(issue_id) or {"id": issue_id}
        return {"id": issue_id, "title": i.get("title"), "url": i.get("url"), "status": i.get("status")} | _issue_counterpart(i, sc)

    links: dict[str, list[str]] = {}
    for ref, pr in prs.items():
        for i in sorted(pr_issue_ids(pr) & set(mine)):
            links.setdefault(i, []).append(ref)

    blockers: dict[str, dict] = {}
    for e in sweep["prs"]:
        pr = prs[e["ref"]]
        blockers[f"pr:{e['ref']}"] = {
            "key": f"pr:{e['ref']}", "kind": "pr", "ref": e["ref"], "title": e["title"], "url": e["url"],
            "draft": e["draft"], "bucket": e["bucket"], "detail": e["detail"], "stale": e["stale"],
            "whose_move": "rollout" if e["ref"] in rollouts else WHOSE_MOVE[e["bucket"]],
            "rollout": rollouts.get(e["ref"]), "issues": sorted(pr_issue_ids(pr) & set(mine)),
            "actions": [] if e["ref"] in rollouts else e["actions"],
        }
    for k, i in sorted(mine.items()):
        outside = [other(b) for b in i.get("blocked_by", []) if is_open(b)]
        outside = [o for o in outside if o["kind"] != "subteam"]
        unrecorded = i.get("status") == "Blocked" and not i.get("blocked_by")
        if outside or unrecorded:
            blockers[f"issue:{k}"] = {"key": f"issue:{k}", "kind": "issue", "id": k, "title": i["title"], "url": i.get("url"),
                                      "status": i.get("status"), "waiting_on": outside, "unrecorded": unrecorded}

    deps: dict[str, dict] = {}

    def dep(direction, mine_ref, title, url, counterpart, state, via):
        key = f"{direction}|{mine_ref}|{counterpart.get('id') or counterpart['label']}"
        deps[key] = {"key": key, "direction": direction, "mine": mine_ref, "title": title, "url": url,
                     "counterpart": counterpart, "state": state, "via": via}

    for k, i in sorted(mine.items()):
        for b in i.get("blocked_by", []):
            o = other(b)
            if is_open(b) and o["kind"] != "subteam":
                dep("waits_on", k, i["title"], i.get("url"), o, o["status"], "linear")
        for b in i.get("blocks", []):
            o = other(b)
            if is_open(b) and o["kind"] != "subteam":
                dep("waited_on_by", k, i["title"], i.get("url"), o, o["status"], "linear")
    for e in sweep["prs"]:
        if e["bucket"] in ("waiting", "overdue"):
            for c in _review_counterparts(e, prs[e["ref"]], config):
                dep("waits_on", e["ref"], e["title"], e["url"], c, e["bucket"], "review")
    for p in inbound:
        kind = person_kind(p["author"], sc)
        if kind != "subteam":
            dep("waited_on_by", p["ref"], p["title"], p["url"], {"kind": kind, "label": p["author"]}, "review requested", "review")

    drift = []
    merged_links: dict[str, list[str]] = {}
    for p in merged:
        for i in sorted(pr_issue_ids(p) & set(mine)):
            merged_links.setdefault(i, []).append(p["ref"])
    for k, i in sorted(mine.items()):
        base = {"id": k, "title": i["title"], "url": i.get("url"), "status": i.get("status")}
        blocked_by = i.get("blocked_by", [])
        open_prs = [prs[r] | {"ref": r} for r in links.get(k, [])]
        if i.get("status") == "Blocked" and not blocked_by:
            drift.append(base | {"rule": "blocked-without-blocker",
                                 "propose": "add a blocked-by relation to what it waits on, or move it out of Blocked"})
        elif i.get("status") == "Blocked" and not any(is_open(b) for b in blocked_by):
            drift.append(base | {"rule": "blocker-resolved", "blockers": blocked_by, "propose": "move it out of Blocked"})
        if i.get("status_type") == "unstarted" and open_prs:
            ready = [p["ref"] for p in open_prs if not p["draft"]]
            drift.append(base | {"rule": "pr-open-issue-not-started", "prs": ready or [p["ref"] for p in open_prs],
                                 "propose": "In Review" if ready else "In Progress"})
        if i.get("status_type") == "started" and i.get("status") != "Blocked" and merged_links.get(k) and not open_prs:
            drift.append(base | {"rule": "prs-merged", "prs": merged_links[k],
                                 "propose": "check the outcome, then mark it Done"})

    fixed = [{"ref": p["ref"], "title": p["title"], "url": p["url"]}
             for p in merged if not pr_mentions_issue(p)]

    prev = previous or {}
    snapshot = {
        "blockers": {k: {"key": k, "title": v["title"], "url": v["url"]} for k, v in blockers.items()},
        "dependencies": {k: {"key": k, "title": v["title"], "url": v["url"], "direction": v["direction"],
                             "counterpart": v["counterpart"], "state": v["state"]} for k, v in deps.items()},
    }
    if previous is None:
        b = {"new": [], "standing": [v for _, v in sorted(blockers.items())], "gone": []}
        d = {"new": [], "changed": [], "standing": [v for _, v in sorted(deps.items())], "gone": []}
    else:
        b = _diff(blockers, prev.get("blockers", {}), track_state=False)
        d = _diff(deps, prev.get("dependencies", {}), track_state=True)
    return {
        "baseline": previous is None,
        "since": prev.get("at"),
        "blockers": {"new": b["new"], "standing": b["standing"], "cleared": b["gone"]},
        "dependencies": {"new": d["new"], "changed": d["changed"], "standing": d["standing"], "resolved": d["gone"]},
        "counts": {
            "blockers": {"new": len(b["new"]), "standing": len(b["standing"]), "cleared": len(b["gone"])},
            "dependencies": {"new": len(d["new"]), "changed": len(d["changed"]), "standing": len(d["standing"]),
                             "resolved": len(d["gone"])},
        },
        "drift": drift,
        "fixed_in_passing": fixed,
        "snapshot": snapshot,
    }
