#!/usr/bin/env python3
"""Rollout state: fetch facts from GitHub with `gh`, then hand them to core.

State lives in $ROLLOUT_STATE_DIR (default ~/.local/state/rollouts):
  <name>.json        the manifest (nodes and their `after` edges)
  <name>.marks.json  steps a person marked done
  requests.jsonl     every Slack review request sent, across all rollouts
Config (deploy workflows, Slack channel map, chase thresholds) is config.json
beside this skill.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import time
from datetime import datetime, timedelta, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import core  # noqa: E402
import page  # noqa: E402

STATE = Path(os.environ.get("ROLLOUT_STATE_DIR", Path.home() / ".local/state/rollouts"))
CONFIG = Path(__file__).resolve().parent.parent / "config.json"

PR_FIELDS = """
number title url state isDraft mergeable reviewDecision additions deletions createdAt
author { login }
mergeCommit { oid }
files(first: 100) { nodes { path } }
reviewRequests(first: 20) { nodes { requestedReviewer {
  __typename ... on Team { slug organization { login } } ... on User { login } } } }
reviews(last: 50) { nodes { author { __typename login } submittedAt state } }
comments(last: 50) { nodes { author { __typename login } createdAt } }
reviewThreads(first: 100) { nodes { isResolved
  first: comments(first: 1) { nodes { author { __typename login } } }
  last: comments(last: 1) { nodes { author { __typename login } createdAt } } } }
commits(last: 1) { nodes { commit { committedDate statusCheckRollup { contexts(first: 100) { nodes {
  __typename ... on CheckRun { name conclusion status } ... on StatusContext { context state } } } } } } }
"""


def gh(*args: str) -> str:
    return subprocess.run(["gh", *args], check=True, capture_output=True, text=True).stdout


def split_ref(ref: str) -> tuple[str, str, int]:
    repo, _, num = ref.partition("#")
    owner, _, name = repo.partition("/")
    return owner, name, int(num)


def login(a) -> str:
    return (a or {}).get("login") or "ghost"


def is_bot(a) -> bool:
    return (a or {}).get("__typename") == "Bot"


def normalise(ref: str, raw: dict) -> dict:
    head = (raw["commits"]["nodes"] or [{}])[0].get("commit", {})
    rollup = head.get("statusCheckRollup") or {}
    checks = []
    for c in (rollup.get("contexts") or {}).get("nodes", []):
        if c["__typename"] == "CheckRun":
            checks.append({"name": c["name"], "conclusion": c["conclusion"], "status": c["status"]})
        else:
            checks.append({"name": c["context"], "state": c["state"]})
    teams, users = [], []
    for r in raw["reviewRequests"]["nodes"]:
        rr = r["requestedReviewer"] or {}
        if rr.get("__typename") == "Team":
            teams.append(f"{rr['organization']['login']}/{rr['slug']}")
        elif rr.get("login"):
            users.append(rr["login"])
    threads, activity = [], []
    for t in raw["reviewThreads"]["nodes"]:
        first = (t["first"]["nodes"] or [{}])[0]
        last = (t["last"]["nodes"] or [{}])[-1]
        threads.append({
            "resolved": t["isResolved"],
            "last_author": login(last.get("author")),
            "started_by_bot": is_bot(first.get("author")),
        })
        if last.get("createdAt"):
            activity.append({"author": login(last.get("author")), "at": last["createdAt"], "bot": is_bot(last.get("author"))})
    for r in raw["reviews"]["nodes"]:
        if r.get("submittedAt"):
            activity.append({"author": login(r.get("author")), "at": r["submittedAt"], "bot": is_bot(r.get("author"))})
    for c in raw["comments"]["nodes"]:
        activity.append({"author": login(c.get("author")), "at": c["createdAt"], "bot": is_bot(c.get("author"))})
    return {
        "repo": ref.split("#")[0],
        "number": raw["number"],
        "title": raw["title"],
        "url": raw["url"],
        "state": raw["state"],
        "draft": raw["isDraft"],
        "author": login(raw["author"]),
        "merge_commit": (raw.get("mergeCommit") or {}).get("oid"),
        "mergeable": raw["mergeable"],
        "review_decision": raw["reviewDecision"],
        "additions": raw["additions"],
        "deletions": raw["deletions"],
        "created_at": raw.get("createdAt"),
        "last_commit_at": head.get("committedDate"),
        "files": [f["path"] for f in raw["files"]["nodes"]],
        "requested_teams": teams,
        "requested_users": users,
        "checks": checks,
        "threads": threads,
        "activity": activity,
    }


def fetch_prs(refs: list[str]) -> dict[str, dict]:
    """One GraphQL call for every PR, keyed by ref."""
    if not refs:
        return {}
    parts = []
    for i, ref in enumerate(refs):
        owner, name, num = split_ref(ref)
        parts.append(f'p{i}: repository(owner: "{owner}", name: "{name}") {{ pullRequest(number: {num}) {{ {PR_FIELDS} }} }}')
    data = json.loads(gh("api", "graphql", "-f", "query={ " + "\n".join(parts) + " }"))["data"]
    return {ref: normalise(ref, data[f"p{i}"]["pullRequest"]) for i, ref in enumerate(refs)}


def latest_success(repo: str, workflow: str, event: str | None = None) -> dict | None:
    q = f"repos/{repo}/actions/workflows/{workflow}/runs?status=success&per_page=1"
    if event:
        q += f"&event={event}"
    runs = json.loads(gh("api", q))["workflow_runs"]
    return runs[0] if runs else None


def includes(repo: str, commit: str, deployed: str) -> bool:
    status = json.loads(gh("api", f"repos/{repo}/compare/{commit}...{deployed}"))["status"]
    return status in ("ahead", "identical")


def fetch_deploys(manifest: dict, prs: dict, config: dict) -> dict:
    nodes = {n["id"]: n for n in manifest["nodes"]}
    wanted: dict[str, set[str]] = {}
    for n in manifest["nodes"]:
        for a in n.get("after", []):
            dep, cond = core.parse_after(a, nodes)
            if cond.startswith("deployed:"):
                wanted.setdefault(dep, set()).add(cond.split(":", 1)[1])
    out: dict[str, dict] = {}
    for dep, envs in wanted.items():
        pr = prs[dep]
        out[dep] = {}
        for env in envs:
            wf = config.get("deploys", {}).get(pr["repo"], {}).get(env)
            if pr["state"] != "MERGED" or not wf:
                out[dep][env] = False if pr["state"] != "MERGED" else None
                continue
            run = latest_success(pr["repo"], wf)
            out[dep][env] = bool(run) and includes(pr["repo"], pr["merge_commit"], run["head_sha"])
    return out


def variable_exists(repo: str, name: str) -> bool:
    try:
        gh("api", f"repos/{repo}/actions/variables/{name}")
    except subprocess.CalledProcessError:
        return False
    return True


def fetch_checks(manifest: dict) -> dict:
    """A step's optional `check` lets the world say it's done:
    {"workflow": {"repo", "file", "event"?, "since"?}} passes on a successful run;
    {"variable": {"repo", "name"}} passes once the Actions variable exists."""
    out = {}
    for n in manifest["nodes"]:
        var = (n.get("check") or {}).get("variable")
        if var:
            out[n["id"]] = variable_exists(var["repo"], var["name"])
        wf = (n.get("check") or {}).get("workflow")
        if not wf:
            continue
        run = latest_success(wf["repo"], wf["file"], wf.get("event"))
        out[n["id"]] = bool(run) and (not wf.get("since") or run["created_at"] >= wf["since"])
    return out


def load_json(p: Path, default):
    return json.loads(p.read_text()) if p.exists() else default


def load_requests() -> list[dict]:
    p = STATE / "requests.jsonl"
    return [json.loads(line) for line in p.read_text().splitlines() if line.strip()] if p.exists() else []


def cmd_status(args) -> int:
    manifest = load_json(STATE / f"{args.name}.json", None)
    if manifest is None:
        sys.exit(f"no manifest at {STATE / (args.name + '.json')}")
    errors = core.validate(manifest)
    if errors:
        sys.exit("invalid manifest:\n  " + "\n  ".join(errors))
    config = load_json(CONFIG, {})
    pr_nodes = {n["id"]: n["pr"] for n in manifest["nodes"] if "pr" in n}
    by_ref = fetch_prs(list(pr_nodes.values()))
    prs = {i: by_ref[ref] for i, ref in pr_nodes.items()}
    facts = {"prs": prs, "deploys": fetch_deploys(manifest, prs, config), "checks": fetch_checks(manifest)}
    marks = load_json(STATE / f"{args.name}.marks.json", {})
    requests = load_requests()
    report = core.evaluate(manifest, facts, marks, requests, config)
    now = datetime.now(timezone.utc)
    summary = {
        "name": manifest["name"],
        "title": manifest.get("title") or manifest["name"],
        "done": sum(n["phase"] == "done" for n in report["nodes"]),
        "total": len(report["nodes"]),
        "yours": sum(core.whose_move(a) == "jml" for n in report["nodes"] for a in n["actions"]),
    }
    (STATE / f"{args.name}.summary.json").write_text(json.dumps(summary) + "\n")
    # Every run refreshes the local copy, which jml can keep open in a browser
    # without the approval each Artifact read or publish needs.
    (STATE / f"{args.name}.html").write_text(page.render(manifest, report, requests, now, refresh_seconds=120))
    write_index(now)
    if args.page:
        Path(args.page).write_text(page.render(manifest, report, requests, now))
    print(json.dumps(report, indent=2) if args.json else render(report))
    return 0


def render(report: dict) -> str:
    lines = [f"rollout {report['name']}: {'complete' if report['complete'] else 'in progress'}, next tick in {report['next_tick_seconds']}s"]
    for n in report["nodes"]:
        label = n.get("ref") or "step"
        extra = f" checks={n['checks']}" if "checks" in n else ""
        extra += f" review={n['review']}" if n.get("review") else ""
        lines.append(f"  [{n['phase']}] {n['id']}: {label} — {n['title']}{extra}")
        for w in n["waiting_on"]:
            lines.append(f"      waits for {w['node']}@{w['condition']}{' (unknown)' if w['unknown'] else ''}")
        for a in n["actions"]:
            lines.append(f"      → {a['kind']}{' (auto)' if a['auto'] else ''}: {json.dumps(a['detail'])}")
    return "\n".join(lines)


def write_index(now: datetime) -> None:
    """Rewrite index.html from the summary each rollout's last status run saved."""
    rows = []
    for p in sorted(STATE.glob("*.summary.json")):
        rows.append(json.loads(p.read_text()))
    (STATE / "index.html").write_text(page.render_index(rows, now))


def cmd_mark(args) -> int:
    p = STATE / f"{args.name}.marks.json"
    marks = load_json(p, {})
    if args.undo:
        marks.pop(args.node, None)
    else:
        marks[args.node] = datetime.now(timezone.utc).isoformat(timespec="seconds")
    p.write_text(json.dumps(marks, indent=2) + "\n")
    return 0


def cmd_record_request(args) -> int:
    STATE.mkdir(parents=True, exist_ok=True)
    entry = {
        "rollout": args.rollout,
        "pr": args.pr,
        "channel": args.channel,
        "permalink": args.permalink,
        "sent_at": args.sent_at or datetime.now(timezone.utc).isoformat(timespec="seconds"),
    }
    with open(STATE / "requests.jsonl", "a") as f:
        f.write(json.dumps(entry) + "\n")
    return 0


def cmd_chase(args) -> int:
    requests = load_requests()
    prs = fetch_prs(sorted({r["pr"] for r in requests}))
    overdue = core.chase(requests, prs, datetime.now(timezone.utc), load_json(CONFIG, {}))
    if args.json:
        print(json.dumps(overdue, indent=2))
    else:
        for o in overdue:
            print(f"{o['pr']} [{o['rollout']}] {o['channel']} — {o['weekday_hours_waited']}h/{o['threshold_hours']}h — {o['title']}")
    return 0


def cmd_wait_checks(args) -> int:
    """Block until the PR's CI and review bots have finished, then exit.

    Run in the background after opening or pushing to a PR: the session is
    woken when this exits and runs a tick at once, rather than at its next
    scheduled wakeup.
    """
    config = load_json(CONFIG, {})
    deadline = time.monotonic() + args.timeout
    while True:
        pr = fetch_prs([args.pr])[args.pr]
        if core.ci_settled(pr, config):
            checks, failing, _ = core._checks(pr, set(config.get("review_gated_checks", [])))
            print(f"{args.pr}: settled (checks {checks}{', failing ' + ', '.join(failing) if failing else ''})")
            return 0
        if time.monotonic() >= deadline:
            print(f"{args.pr}: still running after {args.timeout}s")
            return 1
        time.sleep(args.interval)


def open_prs(me: str) -> tuple[list[str], list[str]]:
    """jml's open PRs as (refs, refs in archived repos), from one search call."""
    q = """query($q: String!) { search(query: $q, type: ISSUE, first: 100) { nodes {
      ... on PullRequest { number repository { nameWithOwner isArchived } } } } }"""
    nodes = json.loads(gh("api", "graphql", "-f", f"query={q}", "-f", f"q=is:pr is:open author:{me}"))["data"]["search"]["nodes"]
    live, archived = [], []
    for n in nodes:
        ref = f"{n['repository']['nameWithOwner']}#{n['number']}"
        (archived if n["repository"]["isArchived"] else live).append(ref)
    return sorted(live), sorted(archived)


def rollout_members() -> dict[str, str]:
    """Every PR in a rollout manifest, mapped to its rollout."""
    out = {}
    for p in sorted(STATE.glob("*.json")):
        if p.name.endswith((".marks.json", ".summary.json")) or p.name.startswith("eod."):
            continue
        m = load_json(p, {})
        for n in m.get("nodes", []):
            if "pr" in n:
                out[n["pr"]] = m.get("name", p.stem)
    return out


def cmd_sweep(args) -> int:
    config = load_json(CONFIG, {})
    now = datetime.now(timezone.utc)
    log = STATE / "eod.jsonl"
    runs = [json.loads(line) for line in log.read_text().splitlines() if line.strip()] if log.exists() else []
    first_run = datetime.fromisoformat(runs[0]["at"]) if runs else now
    live, archived = open_prs(config["me"])
    members = rollout_members()
    refs = [r for r in live if r not in members]
    report = core.eod(fetch_prs(refs), load_requests(), load_json(STATE / "eod.holds.json", {}), now, first_run, config)
    report["in_rollouts"] = {r: members[r] for r in live if r in members}
    report["archived"] = archived
    if args.log:
        STATE.mkdir(parents=True, exist_ok=True)
        with open(log, "a") as f:
            f.write(json.dumps({"at": now.isoformat(timespec="seconds"), "counts": report["counts"],
                                "oldest_ask_hours": report["oldest_ask_hours"],
                                "in_rollouts": len(report["in_rollouts"])}) + "\n")
    if args.json:
        print(json.dumps(report, indent=2))
    else:
        for e in report["prs"]:
            print(f"{e['bucket']:8} {e['ref']}{' (stale draft)' if e['stale'] else ''} — {e['title']}")
        print(f"{len(report['in_rollouts'])} in rollouts, {len(archived)} in archived repos")
    return 0


def cmd_hold(args) -> int:
    p = STATE / "eod.holds.json"
    holds = load_json(p, {})
    if args.undo:
        holds.pop(args.pr, None)
    else:
        days = args.days or load_json(CONFIG, {}).get("eod", {}).get("hold_days", 7)
        today = datetime.now(timezone.utc).date()
        holds[args.pr] = {"until": (today + timedelta(days=days)).isoformat(), "reason": args.reason, "set": today.isoformat()}
    STATE.mkdir(parents=True, exist_ok=True)
    p.write_text(json.dumps(holds, indent=2, sort_keys=True) + "\n")
    return 0


def cmd_list(_args) -> int:
    for p in sorted(STATE.glob("*.json")):
        if not p.name.endswith((".marks.json", ".summary.json")) and not p.name.startswith("eod."):
            print(p.stem)
    return 0


def main() -> int:
    ap = argparse.ArgumentParser()
    sub = ap.add_subparsers(required=True)
    s = sub.add_parser("status")
    s.add_argument("name")
    s.add_argument("--json", action="store_true")
    s.add_argument("--page", metavar="PATH", help="also write the rollout's HTML status page to PATH")
    s.set_defaults(fn=cmd_status)
    m = sub.add_parser("mark")
    m.add_argument("name")
    m.add_argument("node")
    m.add_argument("--undo", action="store_true")
    m.set_defaults(fn=cmd_mark)
    r = sub.add_parser("record-request")
    for flag in ("--rollout", "--pr", "--channel"):
        r.add_argument(flag, required=True)
    r.add_argument("--permalink")
    r.add_argument("--sent-at")
    r.set_defaults(fn=cmd_record_request)
    c = sub.add_parser("chase")
    c.add_argument("--json", action="store_true")
    c.set_defaults(fn=cmd_chase)
    w = sub.add_parser("wait-checks")
    w.add_argument("pr", help="owner/repo#N")
    w.add_argument("--interval", type=int, default=60)
    w.add_argument("--timeout", type=int, default=3600)
    w.set_defaults(fn=cmd_wait_checks)
    sub.add_parser("list").set_defaults(fn=cmd_list)
    e = sub.add_parser("sweep", help="end-of-day buckets for open PRs in no rollout")
    e.add_argument("--json", action="store_true")
    e.add_argument("--log", action="store_true", help="append this run's counts to eod.jsonl")
    e.set_defaults(fn=cmd_sweep)
    h = sub.add_parser("hold", help="stop /eod asking about a PR for a while")
    h.add_argument("pr", help="owner/repo#N")
    h.add_argument("--reason", default="")
    h.add_argument("--days", type=int)
    h.add_argument("--undo", action="store_true")
    h.set_defaults(fn=cmd_hold)
    args = ap.parse_args()
    return args.fn(args)


if __name__ == "__main__":
    sys.exit(main())
