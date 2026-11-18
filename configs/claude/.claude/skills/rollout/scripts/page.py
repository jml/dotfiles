"""Render a rollout's status report as one self-contained HTML page. Pure: no I/O."""

from __future__ import annotations

from datetime import datetime
from html import escape

import core

W, H, GAP_X, GAP_Y, PAD = 232, 66, 72, 18, 12

# Display states, in the order the legend lists them. Each has a glyph and a
# word so that colour never carries the state alone.
STATES = {
    "needs-you": ("!", "needs you"),
    "others": ("→", "with someone else"),
    "session": ("↻", "session working"),
    "proposed": ("?", "proposed"),
    "building": ("⚒", "being built"),
    "waiting": ("…", "waiting"),
    "deploying": ("↑", "deploying"),
    "blocked": ("⏸", "blocked"),
    "done": ("✓", "done"),
    "closed": ("×", "closed"),
}

ACTION_WORDS = {
    "request-review": "Approve the review request",
    "merge": "Merge",
    "mark-ready": "Mark ready for review",
    "do-step": "Do this step",
    "check-failures": "Check these failures",
    "decide": "Decide",
    "rebase": "Rebase",
    "fix-ci": "Fix CI",
    "respond": "Address review threads",
    "decide-scope": "Decide whether this is part of the rollout",
}


def display_state(n: dict) -> str:
    if n["phase"] in ("done", "closed", "deploying", "blocked", "proposed", "building"):
        return n["phase"]
    if n.get("author"):
        return "others"
    moves = {core.whose_move(a) for a in n["actions"]}
    if "jml" in moves:
        return "needs-you"
    if "other" in moves:
        return "others"
    if moves:
        return "session"
    return "waiting"


def short_ref(ref: str) -> str:
    return ref.removeprefix("chainguard-dev/")


def label(n: dict) -> str:
    return short_ref(n["ref"]) if n.get("ref") else n["id"]


def clip(s: str, k: int) -> str:
    return s if len(s) <= k else s[: k - 1] + "…"


def ago(then: str, now: datetime) -> str:
    h = (now - datetime.fromisoformat(then)).total_seconds() / 3600
    return f"{h:.0f} h ago" if h >= 1 else f"{h * 60:.0f} min ago"


def waiting_reason(n: dict, requests: list[dict], now: datetime) -> str:
    deps = ", ".join(
        w["node"] + ("" if w["condition"] in ("merged", "done") else f" ({w['condition']})") for w in n["waiting_on"]
    )
    if n["phase"] == "blocked":
        return "after " + deps
    if n["phase"] == "deploying":
        return "deploying to " + ", ".join(n.get("deploying_to", []))
    if n["kind"] == "step":
        return "happens on its own; its check reports it" if not n["actions"] else ""
    if n["phase"] == "building":
        return "being built" + (f" in session {n['session']}" if n.get("session") else "")
    if n["phase"] == "proposed":
        return "your decision"
    parts = [f"merges after {deps}"] if deps else []
    if n.get("author"):
        parts.insert(0, f"{n['author']}'s PR")
    bots = n.get("bot_review")
    if bots == "pending":
        parts.append("review bots running")
    elif bots == "approved":
        parts.append("bot approval on its way")
    elif bots == "findings":
        parts.append("bot findings open")
    if n.get("checks") == "pending":
        parts.append("checks running")
    if n.get("awaiting_review"):
        parts.append("approval-gated checks: " + ", ".join(n["awaiting_review"]))
    reqs = [r for r in requests if r["pr"] == n["ref"]]
    if reqs:
        last = max(reqs, key=lambda r: r["sent_at"])
        parts.append(f"review asked in {last['channel']} {ago(last['sent_at'], now)}")
    if n.get("review") == "APPROVED":
        parts.append("approved")
    return "; ".join(parts)


def layout(manifest: dict) -> dict[str, tuple[float, float]]:
    """Left-to-right layers by longest path, ordered within a layer by the mean
    position of each node's dependencies to cut crossings."""
    nodes = {n["id"]: n for n in manifest["nodes"]}
    deps = {i: [core.parse_after(a, nodes)[0] for a in n.get("after", [])] for i, n in nodes.items()}
    layer: dict[str, int] = {}

    def depth(i: str) -> int:
        if i not in layer:
            layer[i] = 1 + max((depth(d) for d in deps[i]), default=-1)
        return layer[i]

    order = [n["id"] for n in manifest["nodes"]]
    for i in order:
        depth(i)
    cols: dict[int, list[str]] = {}
    for i in order:
        cols.setdefault(layer[i], []).append(i)
    pos = {i: k for c in cols.values() for k, i in enumerate(c)}
    for _ in range(3):
        for c in sorted(cols)[1:]:
            cols[c].sort(key=lambda i: sum(pos[d] for d in deps[i]) / len(deps[i]) if deps[i] else pos[i])
            pos.update({i: k for k, i in enumerate(cols[c])})
    tallest = max(len(c) for c in cols.values())
    out = {}
    for c, ids in cols.items():
        offset = (tallest - len(ids)) * (H + GAP_Y) / 2
        for k, i in enumerate(ids):
            out[i] = (PAD + c * (W + GAP_X), PAD + offset + k * (H + GAP_Y))
    return out


def svg(manifest: dict, report: dict) -> str:
    nodes = {n["id"]: n for n in manifest["nodes"]}
    by_id = {n["id"]: n for n in report["nodes"]}
    xy = layout(manifest)
    width = max(x for x, _ in xy.values()) + W + PAD
    height = max(y for _, y in xy.values()) + H + PAD
    parts = [f'<svg viewBox="0 0 {width:.0f} {height:.0f}" width="{width:.0f}" height="{height:.0f}" role="img" aria-label="Dependency graph of the rollout">']
    for n in manifest["nodes"]:
        x2, y2 = xy[n["id"]]
        for a in n.get("after", []):
            dep, cond = core.parse_after(a, nodes)
            x1, y1 = xy[dep]
            sx, sy, ex, ey = x1 + W, y1 + H / 2, x2, y2 + H / 2
            mx = (sx + ex) / 2
            met = not any(w["node"] == dep and w["condition"] == cond for w in by_id[n["id"]]["waiting_on"])
            parts.append(f'<path class="edge{" met" if met else ""}" d="M{sx:.0f},{sy:.0f} C{mx:.0f},{sy:.0f} {mx:.0f},{ey:.0f} {ex - 6:.0f},{ey:.0f}" marker-end="url(#arrow)"/>')
            if cond not in ("merged", "done"):
                parts.append(f'<text class="edge-label" x="{mx:.0f}" y="{(sy + ey) / 2 - 5:.0f}" text-anchor="middle">{escape(cond)}</text>')
    parts.append('<defs><marker id="arrow" viewBox="0 0 8 8" refX="7" refY="4" markerWidth="7" markerHeight="7" orient="auto"><path d="M0,0 L8,4 L0,8 z" class="arrowhead"/></marker></defs>')
    for n in report["nodes"]:
        x, y = xy[n["id"]]
        st = display_state(n)
        glyph, word = STATES[st]
        title = n["title"]
        inner = (
            f'<rect class="node s-{st}" x="{x:.0f}" y="{y:.0f}" width="{W}" height="{H}" rx="6"/>'
            f'<text class="n-state s-{st}" x="{x + 12:.0f}" y="{y + 19:.0f}">{glyph} {escape(word)}</text>'
            f'<text class="n-label" x="{x + 12:.0f}" y="{y + 38:.0f}">{escape(label(n))}</text>'
            f'<text class="n-title" x="{x + 12:.0f}" y="{y + 55:.0f}">{escape(clip(title, 34))}</text>'
            f"<title>{escape(label(n))}: {escape(title)}</title>"
        )
        if n.get("url"):
            inner = f'<a href="{escape(n["url"])}" target="_blank" rel="noopener">{inner}</a>'
        parts.append(inner)
    parts.append("</svg>")
    return "".join(parts)


def action_detail(a: dict) -> str:
    d = a["detail"]
    if a["kind"] == "request-review":
        chans = ", ".join(f'{escape(c["channel"])} <span class="why">({escape(c["why"])})</span>' for c in d["channels"]) or "no channel mapped yet"
        return f'<div class="draft"><code>{escape(d["message"])}</code></div><div class="meta">to {chans}</div>'
    if a["kind"] == "check-failures":
        return f'<div class="meta">{escape(", ".join(d["failing"]))}. {escape(d["note"])}.</div>'
    if a["kind"] == "respond":
        return f'<div class="meta">{d["bot_threads"]} bot thread(s), {d["person_threads"]} from people</div>'
    if a["kind"] == "do-step":
        who = {"jml": "you", "external": "someone else; chase them", "session": "the session, once you say so"}.get(d, d)
        return f'<div class="meta">by {escape(who)}</div>'
    if isinstance(d, str):
        return f'<div class="meta">{escape(d)}</div>'
    if isinstance(d, list):
        return f'<div class="meta">{escape(", ".join(map(str, d)))}</div>'
    return ""


def item(n: dict, a: dict) -> str:
    ref = escape(label(n))
    link = f'<a href="{escape(n["url"])}" target="_blank" rel="noopener">{ref}</a>' if n.get("url") else f"<span>{ref}</span>"
    return (
        f'<li class="item"><div class="item-head"><span class="verb">{escape(ACTION_WORDS.get(a["kind"], a["kind"]))}</span> {link}</div>'
        f'<div class="item-title">{escape(n["title"])}</div>{action_detail(a)}</li>'
    )


def others_items(nodes: list[dict], requests: list[dict], now: datetime) -> list[str]:
    """What other people owe the rollout: steps someone else does, PRs someone
    else wrote, and review requests that have gone out and not been answered."""
    out = []

    def li(verb: str, n: dict, meta: str) -> str:
        ref = escape(label(n))
        link = f'<a href="{escape(n["url"])}" target="_blank" rel="noopener">{ref}</a>' if n.get("url") else f"<span>{ref}</span>"
        return (f'<li class="item"><div class="item-head"><span class="verb">{escape(verb)}</span> {link}</div>'
                f'<div class="item-title">{escape(n["title"])}</div><div class="meta">{meta}</div></li>')

    for n in nodes:
        # Only open work can be owed by someone: a merged PR that is deploying
        # has nothing left for a reviewer to do.
        if n["phase"] in ("done", "closed", "deploying", "proposed", "building"):
            continue
        if any(core.whose_move(a) == "other" for a in n["actions"]):
            out.append(li("Someone else does this step", n, "ask them, then mark it done"))
        elif n.get("author"):
            out.append(li(f"Waiting on {n['author']}", n, escape(waiting_reason(n, requests, now))))
        elif n["kind"] == "pr" and n.get("review") != "APPROVED":
            reqs = [r for r in requests if r["pr"] == n["ref"]]
            if reqs:
                last = max(reqs, key=lambda r: r["sent_at"])
                out.append(li("Waiting on review", n, f"asked in {escape(last['channel'])} {ago(last['sent_at'], now)}; /chase lists it once it's overdue"))
    return out


def render(manifest: dict, report: dict, requests: list[dict], now: datetime, refresh_seconds: int | None = None) -> str:
    nodes = report["nodes"]
    done = [n for n in nodes if n["phase"] == "done"]
    yours = [(n, a) for n in nodes for a in n["actions"] if core.whose_move(a) == "jml"]
    sessions = [(n, a) for n in nodes for a in n["actions"] if core.whose_move(a) == "session"]
    others = others_items(nodes, requests, now)
    in_flight = [n for n in nodes if n["phase"] != "done"]
    mins = report["next_tick_seconds"] // 60
    title = manifest.get("title") or manifest["name"]
    context = " · ".join(f'<a href="{escape(u)}" target="_blank" rel="noopener">{escape(short_ref(u.removeprefix("https://github.com/").replace("/pull/", "#")))}</a>' for u in manifest.get("context", []))
    linear = " · ".join(f'<a href="https://linear.app/chainguard/issue/{escape(i)}" target="_blank" rel="noopener">{escape(i)}</a>' for i in manifest.get("linear", []))
    legend = "".join(f'<span class="chip s-{k}">{g} {escape(w)}</span>' for k, (g, w) in STATES.items())
    rows = "".join(
        f'<tr><td class="mono">{escape(label(n))}</td><td><span class="chip s-{display_state(n)}">{STATES[display_state(n)][0]} {escape(STATES[display_state(n)][1])}</span></td>'
        f'<td>{escape(n["title"])}</td><td>{escape(waiting_reason(n, requests, now))}</td></tr>'
        for n in in_flight
    )
    done_list = "".join(f'<li><span class="mono">{escape(label(n))}</span> {escape(n["title"])}</li>' for n in done)
    yours_html = "".join(item(n, a) for n, a in yours) or '<li class="empty">Nothing needs you right now.</li>'
    sessions_html = "".join(item(n, a) for n, a in sessions)
    others_html = "".join(others)
    building_html = "".join(
        f'<li class="item"><div class="item-head"><span class="verb">Being built</span> <span>{escape(n["id"])}</span></div>'
        f'<div class="item-title">{escape(n["title"])}</div>'
        f'<div class="meta">{escape("in session " + n["session"]) if n.get("session") else "no session yet"}</div></li>'
        for n in nodes if n["phase"] == "building"
    )
    follow_html = "".join(
        f'<li><a href="{escape(f["url"])}" target="_blank" rel="noopener">{escape(f.get("ref") or f["url"])}</a> {escape(f.get("title", ""))}</li>'
        if f.get("url") else f'<li>{escape(f.get("title", ""))}</li>'
        for f in manifest.get("follow_ups", [])
    )
    # A local copy opened from disk reloads itself; a published Artifact is
    # republished instead, and its viewer picks up new versions on its own.
    refresh = (
        '<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">\n'
        f'<meta http-equiv="refresh" content="{refresh_seconds}">\n'
    ) if refresh_seconds else ""
    return f"""{refresh}<title>{escape(manifest['name'])} rollout</title>

<meta name="claude:viewport-fit-sides" content="cover">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500&family=IBM+Plex+Sans:wght@400;500;600&family=Space+Grotesk:wght@500;600&display=swap">
<style>
/* Layout: one column; summary, what needs you, the graph (scrolls sideways), then what's in flight. */
:root {{
  --bg: #f6f7f9; --surface: #ffffff; --ink: #17202b; --muted: #5b6776; --line: #d5dbe3; --accent: #2f5bd3;
  --you: #b45309; --you-bg: #fdf0dc; --other: #0f766e; --other-bg: #dcf3ef; --session: #6d43c9; --session-bg: #efe9fb; --wait: #5b6776; --wait-bg: #eceff3;
  --deploy: #1d6fa3; --deploy-bg: #e0eff8; --done: #2f7d4f; --done-bg: #e3f2e8; --closed: #b42318; --closed-bg: #fbe5e3;
  --display: "Space Grotesk", "IBM Plex Sans", system-ui, sans-serif;
  --body: "IBM Plex Sans", system-ui, -apple-system, sans-serif;
  --mono: "IBM Plex Mono", ui-monospace, "SF Mono", Menlo, monospace;
}}
@media (prefers-color-scheme: dark) {{
  :root:not([data-theme="light"]) {{
    color-scheme: dark;
    --bg: #0f141a; --surface: #161d25; --ink: #e4e9ef; --muted: #96a2b0; --line: #2b3541; --accent: #7fa2ff;
    --you: #f2a65a; --you-bg: #3a2814; --other: #5fd4c4; --other-bg: #10322e; --session: #b79bff; --session-bg: #2a2140; --wait: #a3adb9; --wait-bg: #222a33;
    --deploy: #6cb8e6; --deploy-bg: #132c3c; --done: #6fcf97; --done-bg: #15301f; --closed: #f28b82; --closed-bg: #3b1715;
  }}
}}
:root[data-theme="dark"] {{
  color-scheme: dark;
  --bg: #0f141a; --surface: #161d25; --ink: #e4e9ef; --muted: #96a2b0; --line: #2b3541; --accent: #7fa2ff;
  --you: #f2a65a; --you-bg: #3a2814; --other: #5fd4c4; --other-bg: #10322e; --session: #b79bff; --session-bg: #2a2140; --wait: #a3adb9; --wait-bg: #222a33;
  --deploy: #6cb8e6; --deploy-bg: #132c3c; --done: #6fcf97; --done-bg: #15301f; --closed: #f28b82; --closed-bg: #3b1715;
}}
body {{ background: var(--bg); color: var(--ink); font: 15px/1.5 var(--body);
  padding-left: max(16px, env(safe-area-inset-left, 0px)); padding-right: max(16px, env(safe-area-inset-right, 0px)); }}
main {{ max-width: 1100px; margin: 0 auto; padding-block: 28px 48px; display: grid; gap: 28px; }}
a {{ color: var(--accent); }}
a:focus-visible {{ outline: 2px solid var(--accent); outline-offset: 2px; }}
h1, h2 {{ font-family: var(--display); font-weight: 600; text-wrap: balance; margin: 0; }}
h1 {{ font-size: 1.7rem; line-height: 1.2; }}
h2 {{ font-size: 1.05rem; letter-spacing: 0.01em; }}
.eyebrow {{ font: 500 0.75rem/1 var(--mono); letter-spacing: 0.06em; text-transform: uppercase; color: var(--muted); }}
header {{ display: grid; gap: 8px; }}
.summary {{ display: flex; flex-wrap: wrap; gap: 8px 20px; color: var(--muted); font-variant-numeric: tabular-nums; }}
.summary strong {{ color: var(--ink); font-weight: 600; }}
.mono, code {{ font-family: var(--mono); font-size: 0.86em; }}
section {{ display: grid; gap: 12px; }}
ul.items {{ list-style: none; padding: 0; margin: 0; display: grid; gap: 10px; }}
.item {{ background: var(--surface); border: 1px solid var(--line); border-left: 4px solid var(--you); border-radius: 6px; padding: 12px 14px; display: grid; gap: 4px; }}
.session-items .item {{ border-left-color: var(--session); }}
.other-items .item {{ border-left-color: var(--other); }}
ul.plain {{ margin: 0; padding-left: 18px; }}
.item-head {{ display: flex; flex-wrap: wrap; gap: 8px; align-items: baseline; font-weight: 600; }}
.item-head a, .item-head span:not(.verb) {{ font: 500 0.86rem var(--mono); }}
.item-title {{ color: var(--muted); }}
.meta {{ color: var(--muted); font-size: 0.9rem; }}
.why {{ color: var(--muted); }}
.draft code {{ display: inline-block; background: var(--wait-bg); padding: 3px 8px; border-radius: 4px; overflow-wrap: anywhere; }}
.empty {{ color: var(--muted); }}
.graph {{ background: var(--surface); border: 1px solid var(--line); border-radius: 8px; overflow-x: auto; padding: 8px; }}
.legend {{ display: flex; flex-wrap: wrap; gap: 6px; }}
.chip {{ display: inline-flex; gap: 4px; align-items: center; white-space: nowrap; font: 500 0.78rem/1 var(--mono); padding: 4px 8px; border-radius: 999px; }}
svg {{ display: block; max-width: none; }}
svg text {{ font-family: var(--body); }}
.edge {{ fill: none; stroke: var(--line); stroke-width: 1.6; stroke-dasharray: 5 4; }}
.edge.met {{ stroke: var(--muted); stroke-dasharray: none; }}
.arrowhead {{ fill: var(--muted); }}
.edge-label {{ font: 500 11px var(--mono); fill: var(--deploy); paint-order: stroke; stroke: var(--surface); stroke-width: 4px; }}
.node {{ stroke-width: 1.5; }}
.n-state {{ font: 600 11px var(--mono); letter-spacing: 0.04em; text-transform: uppercase; }}
.n-label {{ font: 500 13px var(--mono); fill: var(--ink); }}
.n-title {{ font-size: 12px; fill: var(--muted); }}
a:hover .node, a:focus-visible .node {{ stroke-width: 2.5; }}
.s-needs-you {{ --c: var(--you); --cb: var(--you-bg); }}
.s-session {{ --c: var(--session); --cb: var(--session-bg); }}
.s-others {{ --c: var(--other); --cb: var(--other-bg); }}
.s-proposed {{ --c: var(--you); --cb: var(--surface); }}
.s-building {{ --c: var(--session); --cb: var(--surface); }}
.s-waiting {{ --c: var(--wait); --cb: var(--wait-bg); }}
.s-deploying {{ --c: var(--deploy); --cb: var(--deploy-bg); }}
.s-blocked {{ --c: var(--wait); --cb: var(--surface); }}
.s-done {{ --c: var(--done); --cb: var(--done-bg); }}
.s-closed {{ --c: var(--closed); --cb: var(--closed-bg); }}
.chip {{ color: var(--c); background: var(--cb); }}
.chip.s-blocked {{ box-shadow: inset 0 0 0 1px var(--line); }}
rect.node {{ fill: var(--cb); stroke: var(--c); }}
rect.node.s-blocked {{ stroke-dasharray: 4 3; stroke: var(--muted); }}
rect.node.s-proposed, rect.node.s-building {{ stroke-dasharray: 6 3; }}
.chip.s-proposed, .chip.s-building {{ box-shadow: inset 0 0 0 1px var(--c); }}
text.n-state {{ fill: var(--c); }}
.table-wrap {{ overflow-x: auto; }}
table {{ border-collapse: collapse; width: 100%; min-width: 640px; background: var(--surface); border: 1px solid var(--line); border-radius: 8px; }}
th, td {{ text-align: left; padding: 8px 10px; border-bottom: 1px solid var(--line); vertical-align: top; }}
th {{ font: 500 0.75rem var(--mono); letter-spacing: 0.06em; text-transform: uppercase; color: var(--muted); }}
tr:last-child td {{ border-bottom: none; }}
details summary {{ cursor: pointer; color: var(--muted); }}
details ul {{ margin: 8px 0 0; padding-left: 18px; color: var(--muted); }}
@media (prefers-reduced-motion: no-preference) {{ .node {{ transition: stroke-width 120ms; }} }}
</style>
<main>
<header>
  <div class="eyebrow">Rollout · {escape(manifest['name'])}</div>
  <h1>{escape(title)}</h1>
  <div class="summary">
    <span><strong>{len(done)}</strong> of {len(nodes)} done</span>
    <span><strong>{len(yours)}</strong> need you</span>
    <span>next check in {mins} min</span>
    <span>updated {now.strftime('%d %b %H:%M')} UTC</span>
    {f'<span>{linear}</span>' if linear else ''}
    {f'<span>design: {context}</span>' if context else ''}
  </div>
</header>
<section aria-labelledby="yours"><h2 id="yours">Needs you</h2><ul class="items">{yours_html}</ul></section>
{f'<section aria-labelledby="others"><h2 id="others">With other people</h2><ul class="items other-items">{others_html}</ul></section>' if others_html else ''}
<section aria-labelledby="graph"><h2 id="graph">Dependencies</h2>
  <div class="legend">{legend}</div>
  <div class="graph">{svg(manifest, report)}</div>
  <div class="meta">Solid arrows are met, dashed ones aren't. Labels mark deploy conditions; an unlabelled arrow means merged or done.</div>
</section>
{f'<section aria-labelledby="sess"><h2 id="sess">The session is on it</h2><ul class="items session-items">{sessions_html}{building_html}</ul></section>' if sessions_html or building_html else ''}
<section aria-labelledby="flight"><h2 id="flight">In flight</h2>
  <div class="table-wrap"><table><thead><tr><th>Node</th><th>State</th><th>What</th><th>Waiting on</th></tr></thead><tbody>{rows}</tbody></table></div>
</section>
{f'<section aria-labelledby="fu"><h2 id="fu">Follow-ups filed</h2><ul class="plain">{follow_html}</ul></section>' if follow_html else ''}
{f'<section><details><summary>{len(done)} done</summary><ul>{done_list}</ul></details></section>' if done else ''}
</main>
"""


def render_index(rows: list[dict], now: datetime) -> str:
    """One local page linking every rollout's local page, with its counts."""
    items = "".join(
        f'<tr><td><a href="{escape(r["name"])}.html">{escape(r["name"])}</a></td><td>{escape(r["title"])}</td>'
        f'<td class="num">{r["done"]}/{r["total"]}</td><td class="num">{r["yours"]}</td></tr>'
        for r in rows
    )
    return f"""<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<meta http-equiv="refresh" content="120"><title>Rollouts</title>
<style>
:root {{ --bg: #f6f7f9; --surface: #fff; --ink: #17202b; --muted: #5b6776; --line: #d5dbe3; --accent: #2f5bd3; color-scheme: light dark; }}
@media (prefers-color-scheme: dark) {{ :root {{ --bg: #0f141a; --surface: #161d25; --ink: #e4e9ef; --muted: #96a2b0; --line: #2b3541; --accent: #7fa2ff; }} }}
body {{ background: var(--bg); color: var(--ink); font: 15px/1.5 system-ui, sans-serif; margin: 0; padding: 28px max(16px, 4vw); }}
h1 {{ font-size: 1.5rem; margin: 0 0 4px; }} p {{ color: var(--muted); margin: 0 0 16px; }} a {{ color: var(--accent); }}
table {{ border-collapse: collapse; background: var(--surface); border: 1px solid var(--line); min-width: min(100%, 640px); }}
th, td {{ text-align: left; padding: 8px 12px; border-bottom: 1px solid var(--line); }}
th {{ font-size: 0.75rem; text-transform: uppercase; letter-spacing: 0.06em; color: var(--muted); }}
.num {{ font-variant-numeric: tabular-nums; text-align: right; }}
</style>
<h1>Rollouts</h1><p>Updated {now.strftime('%d %b %H:%M')} UTC. Each page reloads itself.</p>
<table><thead><tr><th>Rollout</th><th>What</th><th class="num">Done</th><th class="num">Need you</th></tr></thead><tbody>{items}</tbody></table>
"""
