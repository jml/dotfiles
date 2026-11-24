"""Tests for pr_map.classify. Run: python3 -m unittest -v (from this directory)."""

import unittest

import pr_map

NOW = "2026-11-24T12:00:00+00:00"


def check(name, conclusion="SUCCESS", status="COMPLETED"):
    return {"__typename": "CheckRun", "name": name, "conclusion": conclusion, "status": status}


def gh_pr(n, checks=(), draft=False, state="OPEN", repo="chainguard-dev/mono", decision="REVIEW_REQUIRED"):
    rollup = {"state": "SUCCESS", "contexts": {"nodes": list(checks) or [check("ci")]}}
    return {
        "number": n, "url": f"u{n}", "title": f"feat: t{n}", "isDraft": draft, "state": state, "mergedAt": None,
        "baseRefName": "main", "headRefName": f"b{n}", "author": {"login": "jml"},
        "repository": {"nameWithOwner": repo}, "mergeable": "MERGEABLE", "reviewDecision": decision,
        "commits": {"nodes": [{"commit": {"statusCheckRollup": rollup}}]},
        "reviewRequests": {"nodes": [{"requestedReviewer": {"slug": "developer-platform-team-write"}}]},
        "latestReviews": {"nodes": []}, "reviewThreads": {"nodes": []},
    }


def model(prs, sweep=(), edges=(), extra=()):
    return {"prs": list(prs), "extra": list(extra), "now": NOW,
            "cfg": {"edges": list(edges), "overrides": {}},
            "sweep": {"prs": list(sweep)}, "rollouts": {}, "chase": [], "requests": {}}


def waiting(n):
    return {"ref": f"chainguard-dev/mono#{n}", "bucket": "waiting", "detail": {}, "actions": []}


class ClassifyTest(unittest.TestCase):
    def owners(self, m):
        return {r.split("#")[1]: c["owner"] for r, c in pr_map.classify(m).items()}

    def test_ci_decides_before_review(self):
        m = model(
            [gh_pr(1), gh_pr(2, [check("ci", "FAILURE")]), gh_pr(3, [check("ci", None, "IN_PROGRESS")]),
             # A check that waits on an approval is review, not CI.
             gh_pr(4, [check("ci"), check("doc-approval", None, "IN_PROGRESS")])],
            sweep=[waiting(n) for n in (1, 2, 3, 4)])
        self.assertEqual(self.owners(m), {"1": "review", "2": "me", "3": "other", "4": "review"})

    def test_blocked_on_an_unlanded_pr_is_never_review(self):
        upstream = gh_pr(9)
        m = model(
            [upstream, gh_pr(10), gh_pr(11, draft=True), gh_pr(12)],
            sweep=[waiting(n) for n in (9, 10, 11, 12)],
            edges=[{"from": "chainguard-dev/mono#9", "to": "chainguard-dev/mono#10", "kind": "stack", "label": ""},
                   {"from": "chainguard-dev/mono#9", "to": "chainguard-dev/mono#11", "kind": "deploy", "label": ""},
                   # A soft edge (cites, would conflict) doesn't hold review back.
                   {"from": "chainguard-dev/mono#9", "to": "chainguard-dev/mono#12", "kind": "soft", "label": ""}])
        got = pr_map.classify(m)
        self.assertEqual({r.split("#")[1]: c["owner"] for r, c in got.items()}, {"9": "review", "10": "me", "11": "other", "12": "review"})
        self.assertIn("move back to draft", got["chainguard-dev/mono#10"]["why"])

    def test_a_merged_upstream_releases_its_dependents(self):
        merged = gh_pr(9, state="MERGED")
        m = model([gh_pr(10)], sweep=[waiting(10)], extra=[merged],
                  edges=[{"from": "chainguard-dev/mono#9", "to": "chainguard-dev/mono#10", "kind": "stack", "label": ""}])
        self.assertEqual(self.owners(m), {"9": "done", "10": "review"})


if __name__ == "__main__":
    unittest.main()
