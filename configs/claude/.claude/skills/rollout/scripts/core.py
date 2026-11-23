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
