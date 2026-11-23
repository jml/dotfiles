#!/usr/bin/env python3
"""List the GitHub pull requests and issues where someone waits on jml.

Prints JSON: {"me", "since", "items": [...]}, oldest wait first. An item is
open when jml is a requested reviewer by name (not through a team), or when a
person @-mentioned jml after jml last commented or reviewed there.
"""

import argparse
import json
import re
import subprocess
import sys
from datetime import date, datetime, timedelta, timezone

STALE_DAYS = 30


def is_bot(login):
    # gh's search API spells app authors "dependabot[bot]"; `gh pr view` spells them "app/dependabot".
    return login.startswith("app/") or login.endswith("[bot]") or login.endswith("-bot") or login in {"github-actions", "chainguard", "chainguard-review", "linear"}


def classify(item, me, now):
    """Decide whether one PR or issue waits on `me`. Pure: no I/O.

    `item` holds: ref, url, title, author, is_pr, is_draft, updated_at,
    review_decision, requested (logins asked by name), and events (comments
    and reviews as {author, at, body, url}). Returns None when nothing waits.
    """
    mention = re.compile(r"(?<![\w-])@" + re.escape(me) + r"(?![\w-])", re.IGNORECASE)
    events = sorted(item["events"], key=lambda e: e["at"])
    my_last = max((e["at"] for e in events if e["author"] == me), default="")
    asks = [
        e for e in events
        if e["author"] != me and not is_bot(e["author"]) and e["at"] > my_last and mention.search(e["body"] or "")
    ]
    requested = me in item["requested"]
    if not asks and not requested:
        return None
    if not asks and is_bot(item["author"]):
        kind = "bot-review"
    elif asks:
        kind = "asked"
    else:
        kind = "review"
    since = asks[0]["at"] if asks else item["updated_at"]
    updated = datetime.fromisoformat(item["updated_at"].replace("Z", "+00:00"))
    return {
        "ref": item["ref"],
        "url": item["url"],
        "title": item["title"],
        "author": item["author"],
        "is_pr": item["is_pr"],
        "is_draft": item["is_draft"],
        "review_decision": item["review_decision"],
        "kind": kind,
        "waiting_since": since,
        "stale": (now - updated).days >= STALE_DAYS,
        "asks": [
            {"author": a["author"], "at": a["at"], "url": a["url"], "excerpt": excerpt(a["body"], mention)}
            for a in asks[-2:]
        ],
    }


def excerpt(body, mention, width=280):
    """The sentence or two around the first @-mention, on one line."""
    text = " ".join((body or "").split())
    m = mention.search(text)
    start = max(0, (m.start() if m else 0) - 40)
    return ("…" if start else "") + text[start:start + width] + ("…" if start + width < len(text) else "")


# I/O below this line.

def gh_json(*args):
    out = subprocess.run(["gh", *args], check=True, capture_output=True, text=True).stdout
    return json.loads(out) if out.strip() else None


def search(query):
    return gh_json("api", "-X", "GET", "search/issues", "-f", f"q={query}", "-f", "per_page=100", "-q", ".items")


def fetch(repo, number, is_pr):
    fields = "author,title,url,updatedAt,comments" + (",isDraft,reviewDecision,reviews,reviewRequests" if is_pr else "")
    d = gh_json("pr" if is_pr else "issue", "view", str(number), "--repo", repo, "--json", fields)
    events = [
        {"author": c["author"]["login"], "at": c["createdAt"], "body": c["body"], "url": c.get("url", d["url"])}
        for c in d.get("comments", [])
    ]
    events += [
        {"author": r["author"]["login"], "at": r["submittedAt"], "body": r["body"], "url": d["url"]}
        for r in d.get("reviews", [])
        if r.get("submittedAt")
    ]
    return {
        "ref": f"{repo}#{number}",
        "url": d["url"],
        "title": d["title"],
        "author": d["author"]["login"],
        "is_pr": is_pr,
        "is_draft": d.get("isDraft", False),
        "updated_at": d["updatedAt"],
        "review_decision": d.get("reviewDecision") or None,
        "requested": [r["login"] for r in d.get("reviewRequests", []) if r.get("login")],
        "events": events,
    }


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--days", type=int, default=14, help="how far back to look for mentions")
    a = p.parse_args()
    now = datetime.now(timezone.utc)
    since = (date.today() - timedelta(days=a.days)).isoformat()
    me = gh_json("api", "user")["login"]

    hits = search("is:pr is:open archived:false user-review-requested:@me")
    hits += search(f"is:open archived:false mentions:@me -author:@me updated:>={since}")
    seen, items = set(), []
    for h in hits:
        repo = h["repository_url"].split("/repos/", 1)[1]
        key = (repo, h["number"])
        if key in seen:
            continue
        seen.add(key)
        try:
            c = classify(fetch(repo, h["number"], "pull_request" in h), me, now)
        except subprocess.CalledProcessError as e:
            print(f"skipping {repo}#{h['number']}: {e.stderr.strip()}", file=sys.stderr)
            continue
        if c:
            items.append(c)
    items.sort(key=lambda i: i["waiting_since"])
    json.dump({"me": me, "since": since, "items": items}, sys.stdout, indent=1)
    print()


if __name__ == "__main__":
    main()
