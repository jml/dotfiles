#!/usr/bin/env python3
"""Build a DAG of jml's open PRs and say who owes the next move on each.

  pr_map.py render            fetch, classify, write the page; print what changed since the last render
  pr_map.py watch [SECONDS]   poll (default every 600 s) until a PR's status changes, then exit

State lives in ~/.local/state/pr-map: edges.json (dependencies and overrides that
GitHub doesn't record), pr-map.html (the page), pr-map.json (the last
classification), pr-map.fp (fingerprints the watcher compares against) and
artifact-url (where the page is published).

I/O lives in the fetch_* functions and main(); classify(), fingerprint() and
render() are pure over the fetched model.
"""
import datetime as dt
import hashlib
import html
import json
import os
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
ROLLOUT = os.path.expanduser("~/.claude/skills/rollout/scripts/rollout.py")
STATE = os.path.expanduser("~/.local/state/rollouts")
MAP_STATE = os.path.expanduser("~/.local/state/pr-map")
PAGE = os.path.join(MAP_STATE, "pr-map.html")
CLASSES = os.path.join(MAP_STATE, "pr-map.json")
FP = os.path.join(MAP_STATE, "pr-map.fp")
EDGES = os.path.join(MAP_STATE, "edges.json")
ME = "jml"
BOTS = {"chainguard", "github-actions", "copilot-pull-request-reviewer"}
# Checks that wait on a reviewer's approval, so a pending one means "waiting on review", not CI.
REVIEW_GATED = {"doc-approval", "cg-codeowners-check", "Checks review gate"}

PR_FIELDS = """
  number url title isDraft state mergedAt baseRefName headRefName
  author { login }
  repository { nameWithOwner }
  mergeable reviewDecision
  commits(last: 1) { nodes { commit { statusCheckRollup { state contexts(first: 100) { nodes {
    __typename ... on CheckRun { name conclusion status } ... on StatusContext { context state } } } } } } }
  reviewRequests(first: 20) { nodes { requestedReviewer { ... on User { login } ... on Team { slug } } } }
  latestReviews(first: 20) { nodes { author { login } state } }
  reviewThreads(first: 50) { nodes { isResolved comments(last: 1) { nodes { author { login } } } } }
"""


# ---------- I/O ----------

def gh_graphql(query):
    out = subprocess.run(["gh", "api", "graphql", "-f", f"query={query}"],
                         check=True, capture_output=True, text=True).stdout
    return json.loads(out)["data"]


def fetch_open():
    q = 'query { search(query: "is:pr is:open author:@me archived:false", type: ISSUE, first: 100) { nodes { ... on PullRequest { %s } } } }' % PR_FIELDS
    return [n for n in gh_graphql(q)["search"]["nodes"] if n]


def fetch_refs(refs):
    """Fetch PRs named by edges that the open search did not return."""
    if not refs:
        return []
    parts = []
    for i, ref in enumerate(sorted(refs)):
        repo, num = ref.split("#")
        owner, name = repo.split("/")
        parts.append('r%d: repository(owner: "%s", name: "%s") { pullRequest(number: %s) { %s } }' % (i, owner, name, num, PR_FIELDS))
    data = gh_graphql("query { %s }" % " ".join(parts))
    return [v["pullRequest"] for v in data.values() if v and v.get("pullRequest")]


def run_json(args):
    out = subprocess.run(["python3", ROLLOUT, *args], capture_output=True, text=True)
    if out.returncode != 0:
        return None
    return json.loads(out.stdout)


def fetch_requests():
    latest = {}
    path = os.path.join(STATE, "requests.jsonl")
    if os.path.exists(path):
        for line in open(path):
            try:
                r = json.loads(line)
            except ValueError:
                continue
            if r.get("pr") and (r["pr"] not in latest or r.get("sent_at", "") > latest[r["pr"]].get("sent_at", "")):
                latest[r["pr"]] = r
    return latest


def fetch_model(with_rollouts=True):
    cfg = json.load(open(EDGES)) if os.path.exists(EDGES) else {"edges": [], "overrides": {}}
    prs = fetch_open()
    have = {ref(p) for p in prs}
    wanted = {e[k] for e in cfg["edges"] for k in ("from", "to")}
    extra = fetch_refs(wanted - have)
    model = {"prs": prs, "extra": extra, "cfg": cfg,
             "now": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds")}
    if with_rollouts:
        sweep = run_json(["sweep", "--json"]) or {"prs": [], "in_rollouts": {}}
        names = sorted(set(sweep.get("in_rollouts", {}).values()))
        model["sweep"] = sweep
        model["rollouts"] = {n: run_json(["status", n, "--json"]) for n in names}
        model["chase"] = run_json(["chase", "--json"]) or []
        model["requests"] = fetch_requests()
    return model


# ---------- pure ----------

def ref(p):
    return f'{p["repository"]["nameWithOwner"]}#{p["number"]}'


def short_repo(r):
    return r.split("/")[1].replace("internal-analytics", "ia").replace("sandbox-of-power", "sop")


def rollup(p):
    n = p["commits"]["nodes"]
    s = n and n[0]["commit"].get("statusCheckRollup")
    return s["state"] if s else None


def ci_checks(p):
    """Return (failing, pending) check names on the PR's head commit."""
    n = p["commits"]["nodes"]
    s = n and n[0]["commit"].get("statusCheckRollup")
    failing, pending = [], []
    for c in (s or {}).get("contexts", {}).get("nodes", []):
        if (c.get("name") or c.get("context")) in REVIEW_GATED:
            continue
        if c["__typename"] == "CheckRun":
            if c["status"] != "COMPLETED":
                pending.append(c["name"])
            elif c["conclusion"] in ("FAILURE", "TIMED_OUT", "CANCELLED", "ACTION_REQUIRED", "STARTUP_FAILURE"):
                failing.append(c["name"])
        elif c["state"] in ("PENDING", "EXPECTED"):
            pending.append(c["context"])
        elif c["state"] in ("FAILURE", "ERROR"):
            failing.append(c["context"])
    return sorted(set(failing)), sorted(set(pending))


def fingerprint(model):
    """What counts as a status change: state, draft, review, CI, mergeability, threads, deploy phase."""
    rows = []
    for p in model["prs"] + model["extra"]:
        rows.append([ref(p), p["state"], p["isDraft"], p["reviewDecision"], rollup(p), p["mergeable"], p["baseRefName"],
                     sorted(f'{r["author"]["login"]}:{r["state"]}' for r in p["latestReviews"]["nodes"] if r["author"]),
                     sum(1 for t in p["reviewThreads"]["nodes"] if not t["isResolved"]),
                     sorted((r["requestedReviewer"] or {}).get("login") or (r["requestedReviewer"] or {}).get("slug") or "" for r in p["reviewRequests"]["nodes"])])
    for name, st in sorted((model.get("rollouts") or {}).items()):
        for n in (st or {}).get("nodes", []):
            rows.append([name, n["id"], n.get("phase"), n.get("deploying_to")])
    return hashlib.sha256(json.dumps(sorted(rows, key=str), default=str).encode()).hexdigest()


def hours_since(iso, now):
    t = dt.datetime.fromisoformat(iso.replace("Z", "+00:00"))
    return (dt.datetime.fromisoformat(now) - t).total_seconds() / 3600


def ago(iso, now):
    h = hours_since(iso, now)
    return f"{h:.0f} h ago" if h < 48 else f"{h / 24:.0f} d ago"


def reviewers(p):
    out = []
    for r in p["reviewRequests"]["nodes"]:
        rv = r["requestedReviewer"] or {}
        if rv.get("login"):
            out.append(rv["login"])
        elif rv.get("slug"):
            out.append(rv["slug"].replace("-team-write", "").replace("-admins", "").replace("-write", ""))
    return sorted(set(out))


def approvals(p):
    return sorted(r["author"]["login"] for r in p["latestReviews"]["nodes"]
                  if r["author"] and r["state"] == "APPROVED" and r["author"]["login"] not in BOTS)


def review_why(p, model):
    r = ref(p)
    bits = []
    rv = reviewers(p)
    if rv:
        bits.append("reviewers: " + ", ".join(rv))
    ap = approvals(p)
    if ap:
        bits.append("approved by " + ", ".join(ap) + ", still needs an owner's approval")
    req = model.get("requests", {}).get(r)
    if req:
        bits.append(f'asked in {req["channel"]} {ago(req["sent_at"], model["now"])}')
    for c in model.get("chase", []):
        if c["pr"] == r:
            bits.append(f'overdue: {c["weekday_hours_waited"]:.0f} weekday h against a {c["threshold_hours"]:.0f} h threshold')
    return "; ".join(bits) or "review required"


def classify(model):
    """Return {ref: {owner, why, rollout}} with owner in me | review | other | done."""
    over = model["cfg"].get("overrides", {})
    sweep = {s["ref"]: s for s in model.get("sweep", {}).get("prs", [])}
    roll_nodes = {}
    for name, st in (model.get("rollouts") or {}).items():
        for n in (st or {}).get("nodes", []):
            if n.get("ref"):
                roll_nodes[n["ref"]] = (name, n)
    out = {}
    for p in model["extra"]:
        r = ref(p)
        if p["state"] == "MERGED":
            phase = roll_nodes.get(r, (None, {}))[1].get("phase")
            if phase == "deploying":
                to = ", ".join(roll_nodes[r][1].get("deploying_to") or [])
                out[r] = {"owner": "other", "why": f"Merged; deploying to {to}."}
            else:
                out[r] = {"owner": "done", "why": "Merged " + (p["mergedAt"] or "")[:10]}
        elif p["state"] == "CLOSED":
            out[r] = {"owner": "done", "why": "Closed unmerged"}
        else:
            out[r] = {"owner": "other", "why": f'{p["author"]["login"]}\'s PR' + (" (draft)" if p["isDraft"] else "")}
    for p in model["prs"]:
        r = ref(p)
        if r in over:
            out[r] = dict(over[r])
            continue
        if r in roll_nodes:
            name, n = roll_nodes[r]
            waits = [f'{w["node"]} {w["condition"]}' for w in n.get("waiting_on", [])]
            if any(a["kind"] == "mark-draft" for a in n.get("actions", [])):
                out[r] = {"owner": "me", "why": "in review but waits on " + " and ".join(waits) + ": move back to draft", "rollout": name}
            elif p["isDraft"] and waits:
                out[r] = {"owner": "other", "why": "draft until " + " and ".join(waits), "rollout": name}
            elif n.get("actions"):
                out[r] = {"owner": "me", "why": "; ".join(a.get("summary") or a.get("kind") or str(a) for a in n["actions"]), "rollout": name}
            elif n.get("failing"):
                out[r] = {"owner": "me", "why": "failing: " + ", ".join(n["failing"]), "rollout": name}
            elif p["isDraft"] and waits:
                out[r] = {"owner": "other", "why": "draft held until " + " and ".join(waits), "rollout": name}
            elif p["reviewDecision"] == "REVIEW_REQUIRED" or n.get("awaiting_review"):
                why = review_why(p, model)
                if n.get("awaiting_review"):
                    why += "; review-gated checks still waiting: " + ", ".join(n["awaiting_review"])
                if waits:
                    why += "; merge also waits on " + " and ".join(waits)
                out[r] = {"owner": "review", "why": why, "rollout": name}
            elif waits:
                out[r] = {"owner": "other", "why": "waits on " + " and ".join(waits), "rollout": name}
            else:
                out[r] = {"owner": "me", "why": f'rollout phase {n.get("phase")}', "rollout": name}
            continue
        s = sweep.get(r, {})
        b, d = s.get("bucket"), s.get("detail") or {}
        if b == "fix":
            bits = []
            if d.get("failing"):
                bits.append("failing: " + ", ".join(d["failing"]))
            if d.get("conflicting"):
                bits.append("merge conflict")
            if d.get("bot_findings"):
                bits.append(f'{d["bot_findings"]} open review-bot finding(s)')
            out[r] = {"owner": "me", "why": "; ".join(bits) or "needs a fix"}
        elif b == "reply":
            out[r] = {"owner": "me", "why": "a reviewer is waiting on your reply"}
        elif b == "merge":
            out[r] = {"owner": "me", "why": "approved and green: merge it"}
        elif b == "ask":
            out[r] = {"owner": "me", "why": "green and ready, but no review request sent"}
        elif b == "ready":
            out[r] = {"owner": "me", "why": "green draft: mark ready for review?"}
        elif b in ("waiting", "overdue", "bots"):
            out[r] = {"owner": "review", "why": review_why(p, model)}
        elif b == "stacked":
            out[r] = {"owner": "review", "why": review_why(p, model),
                      "design_doc": ", ".join(x.replace("chainguard-dev/", "") for x in d.get("design_doc") or [])}
        elif b == "blocked" and any(a["kind"] == "mark-draft" for a in s.get("actions", [])):
            out[r] = {"owner": "me", "why": "in review but stacked on " + ", ".join(d.get("builds_on") or [d.get("base", "?")]) + ": move back to draft"}
        elif b == "blocked" and d.get("todos"):
            out[r] = {"owner": "me", "why": "to-do before merge: " + "; ".join(d["todos"])}
        elif b == "blocked" and d.get("session"):
            out[r] = {"owner": "other", "why": f'session "{d["session"]}" is working on it'}
        elif b == "blocked":
            out[r] = {"owner": "other", "why": "builds on " + ", ".join(d.get("builds_on") or [d.get("base", "?")])}
        elif b == "owned":
            out[r] = {"owner": "other", "why": f'session "{d.get("session")}" will send the review request'}
        elif b == "held":
            out[r] = {"owner": "other", "why": f'held until {d.get("until")}: {d.get("reason")}'}
        elif b == "running":
            out[r] = {"owner": "other", "why": "CI still running"}
        else:
            out[r] = {"owner": "other", "why": b or "no status"}
    # A PR stacked on, or waiting for the deploy of, an unlanded PR is rebased
    # once that lands, so reviewing it now is wasted: it belongs in draft.
    # A stack edge is satisfied once its upstream merges; a deploy edge only
    # once the upstream has finished deploying (owner "done").
    hard = {}
    for e in model["cfg"]["edges"]:
        if e["kind"] in ("stack", "deploy"):
            hard.setdefault(e["to"], []).append((e["from"], e["kind"]))

    def unlanded(f, kind):
        o = out.get(f, {})
        if o.get("owner") == "done":
            return False
        return not (kind == "stack" and o.get("why", "").startswith("Merged"))

    for p in model["prs"]:
        r = ref(p)
        if r in over:
            continue
        live = [f for f, kind in hard.get(r, []) if unlanded(f, kind)]
        if not live or out.get(r, {}).get("why", "").startswith("held until"):
            continue
        if not p["isDraft"]:
            out[r] = {**out[r], "owner": "me", "why": "in review but waits on " + ", ".join(live) + ": move back to draft"}
        elif out[r]["owner"] != "other":
            out[r] = {**out[r], "owner": "other", "why": "draft until " + ", ".join(live) + " lands"}
    # A PR with red or unfinished CI is not waiting on review: failing CI is
    # jml's to fix, and pending CI waits on the checks.
    for p in model["prs"]:
        r = ref(p)
        if out.get(r, {}).get("owner") != "review":
            continue
        failing, pending = ci_checks(p)
        if out[r].get("design_doc") and set(failing) <= {"pr-hygiene"}:
            # pr-hygiene fails only because the design doc isn't on main yet.
            out[r] = {"owner": "other", "why": "pr-hygiene fails until design doc " + out[r]["design_doc"] + " merges, so review waits on that doc"}
        elif failing:
            out[r] = {**out[r], "owner": "me", "why": "failing: " + ", ".join(failing) + ". Review so far: " + out[r]["why"]}
        elif pending:
            out[r] = {**out[r], "owner": "other", "why": "CI still running: " + ", ".join(pending[:4]) + (f" and {len(pending) - 4} more" if len(pending) > 4 else "") + ". Review so far: " + out[r]["why"]}
    return out


OWNER_LABEL = {"me": "Waiting on me", "review": "Waiting on review", "other": "Waiting on something else", "done": "Done"}


def mermaid(model, cls):
    allp = {ref(p): p for p in model["prs"] + model["extra"]}
    nid = {r: "n" + hashlib.md5(r.encode()).hexdigest()[:8] for r in allp}
    lines = ["flowchart LR"]
    by_repo = {}
    for r in allp:
        by_repo.setdefault(r.split("#")[0], []).append(r)
    for repo, refs in sorted(by_repo.items()):
        lines.append(f'  subgraph {short_repo(repo).replace("-", "_")}["{repo.split("/")[1]}"]')
        for r in sorted(refs):
            p = allp[r]
            t = p["title"]
            t = t.split(": ", 1)[1] if ": " in t[:40] else t
            t = (t[:44] + "…") if len(t) > 45 else t
            t = t.replace('"', "'").replace("<", "").replace(">", "")
            draft = " (draft)" if p["isDraft"] and p["state"] == "OPEN" else ""
            lines.append(f'    {nid[r]}["<b>#{p["number"]}</b>{draft}<br/>{t.replace("&", "and")}"]:::{cls.get(r, {}).get("owner", "other")}')
        lines.append("  end")
    arrows = {"stack": "-->", "deploy": "==>", "design": "-->", "soft": "-.->"}
    for e in model["cfg"]["edges"]:
        if e["from"] in nid and e["to"] in nid:
            lines.append(f'  {nid[e["from"]]} {arrows[e["kind"]]}|{e["label"]}| {nid[e["to"]]}')
    lines += [
        "  classDef me fill:#f6c76b,stroke:#9a6410,color:#2b1d05,stroke-width:2px",
        "  classDef review fill:#a9c8f2,stroke:#2f5f9e,color:#0d1f38",
        "  classDef other fill:#d9dbe0,stroke:#7c8290,color:#23262d",
        "  classDef done fill:#b9e0c0,stroke:#3f7d4b,color:#10301a,stroke-dasharray:4 3",
    ]
    return "\n".join(lines)


def render(model, cls):
    allp = {ref(p): p for p in model["prs"] + model["extra"]}
    mine = {ref(p) for p in model["prs"]}
    groups = {k: [] for k in OWNER_LABEL}
    for r in sorted(cls, key=lambda r: (r.split("#")[0], -int(r.split("#")[1]))):
        if r in mine or cls[r]["owner"] != "done":
            groups[cls[r]["owner"]].append(r)

    def item(r):
        p = allp[r]
        c = cls[r]
        tags = []
        if p["isDraft"] and p["state"] == "OPEN":
            tags.append('<span class="tag">draft</span>')
        if r not in mine and p["state"] == "OPEN":
            tags.append(f'<span class="tag">{html.escape(p["author"]["login"])}</span>')
        if c.get("rollout"):
            tags.append(f'<span class="tag roll">rollout: {html.escape(c["rollout"])}</span>')
        ci = rollup(p)
        if ci and p["state"] == "OPEN":
            tags.append(f'<span class="ci ci-{ci.lower()}">CI {ci.lower()}</span>')
        repo = p["repository"]["nameWithOwner"].split("/")[1]
        return (f'<li><div class="row"><a href="{p["url"]}">{html.escape(repo)}#{p["number"]}</a>'
                f'<span class="title">{html.escape(p["title"])}</span></div>'
                f'<div class="why">{html.escape(c["why"])}</div><div class="tags">{"".join(tags)}</div></li>')

    def section(k, cls_name):
        items = groups[k]
        body = "".join(item(r) for r in items) or '<li class="empty">Nothing here.</li>'
        return f'<section class="list {cls_name}"><h2>{OWNER_LABEL[k]} <span class="count">{len(items)}</span></h2><ul>{body}</ul></section>'

    now = dt.datetime.fromisoformat(model["now"]).strftime("%a %d %b %Y, %H:%M UTC")
    tpl = open(os.path.join(HERE, "template.html")).read()
    return (tpl.replace("{{NOW}}", now)
               .replace("{{N_ME}}", str(len(groups["me"])))
               .replace("{{N_REVIEW}}", str(len(groups["review"])))
               .replace("{{N_OTHER}}", str(len(groups["other"])))
               .replace("{{N_TOTAL}}", str(len(mine)))
               .replace("{{MERMAID}}", html.escape(mermaid(model, cls), quote=False))
               .replace("{{ME}}", section("me", "me"))
               .replace("{{REVIEW}}", section("review", "review"))
               .replace("{{OTHER}}", section("other", "other")))


def changes(old, new):
    """What moved between two classifications, for the report after a render."""
    out = []
    for r in sorted(set(old) | set(new)):
        o, n = old.get(r), new.get(r)
        if not n:
            out.append({"pr": r, "from": o["owner"], "to": "gone"})
        elif not o or o["owner"] != n["owner"] or o["why"] != n["why"]:
            out.append({"pr": r, "from": o["owner"] if o else "new", "to": n["owner"], "why": n["why"]})
    return out


def main():
    cmd = sys.argv[1] if len(sys.argv) > 1 else "render"
    os.makedirs(MAP_STATE, exist_ok=True)
    if cmd == "render":
        model = fetch_model()
        cls = classify(model)
        old = json.load(open(CLASSES))["classes"] if os.path.exists(CLASSES) else {}
        open(PAGE, "w").write(render(model, cls))
        cheap = {k: v for k, v in model.items() if k in ("prs", "extra", "cfg", "now")}
        json.dump({"full": fingerprint(model), "cheap": fingerprint(cheap)}, open(FP, "w"))
        json.dump({"classes": cls}, open(CLASSES, "w"), indent=1)
        counts = {k: sum(c["owner"] == k for r, c in cls.items() if r in {ref(p) for p in model["prs"]}) for k in OWNER_LABEL}
        print(json.dumps({"page": PAGE, "counts": counts, "changes": changes(old, cls)}, indent=1))
    elif cmd == "watch":
        interval = int(sys.argv[2]) if len(sys.argv) > 2 else 600
        old = json.load(open(FP))
        tick = 0
        while True:
            time.sleep(interval)
            tick += 1
            # The rollout sweep and status calls cost many GraphQL points, so only
            # every third tick compares deploy phases; the others compare the PRs.
            kind = "full" if tick % 3 == 0 else "cheap"
            try:
                fp = fingerprint(fetch_model(with_rollouts=kind == "full"))
            except Exception as e:  # transient gh failures: try again next tick
                print(f"fetch failed: {e}", file=sys.stderr)
                continue
            if fp != old[kind]:
                print(f"changed at {dt.datetime.now(dt.timezone.utc).isoformat(timespec='seconds')}")
                return
    else:
        sys.exit(f"unknown command {cmd!r}; use render or watch")


if __name__ == "__main__":
    main()
