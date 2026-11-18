"""Tests for core.evaluate and core.chase. Run: python3 -m unittest -v (from this directory)."""

import unittest
from datetime import datetime, timezone

import core

CONFIG = {
    "channels": {
        "teams": {"chainguard-dev/data-analytics-admins": "#chan-data"},
        "paths": [{"repo": "chainguard-dev/mono", "prefix": "public/go-driftlessaf/", "channel": "#chan-lib"}],
        "repos": {},
    },
    "chase": {"base_hours": 4, "lines_per_hour": 250, "max_hours": 16, "ignore_authors": ["chainguard"]},
}


def pr(**kw):
    base = {
        "repo": "chainguard-dev/mono", "number": 1, "title": "t", "url": "https://github.com/chainguard-dev/mono/pull/1",
        "state": "OPEN", "draft": False, "author": "jml", "merge_commit": None, "mergeable": "MERGEABLE",
        "review_decision": "REVIEW_REQUIRED", "additions": 10, "deletions": 0, "files": [],
        "requested_teams": [], "requested_users": [], "checks": [{"name": "ci", "conclusion": "SUCCESS", "status": "COMPLETED"}],
        "threads": [], "activity": [],
    }
    return base | kw


MANIFEST = {
    "name": "loader",
    "nodes": [
        {"id": "infra", "pr": "chainguard-dev/data-analytics-infra#21", "after": ["bootstrap"]},
        {"id": "bootstrap", "step": "Owner applies bootstrap/"},
        {"id": "seed", "step": "Dispatch the seed", "after": ["infra@deployed:prod"]},
        {"id": "models", "pr": "chainguard-dev/internal-analytics#517", "after": ["seed"]},
    ],
}


class EvaluateTest(unittest.TestCase):
    def test_mid_rollout(self):
        facts = {
            "prs": {
                "infra": pr(repo="chainguard-dev/data-analytics-infra", url="u21", title="infra",
                            requested_teams=["chainguard-dev/data-analytics-admins"]),
                "models": pr(repo="chainguard-dev/internal-analytics", url="u517", title="models",
                             checks=[{"name": "dbt", "conclusion": "FAILURE", "status": "COMPLETED"}]),
            },
            "deploys": {"infra": {"prod": False}},
            "checks": {},
        }
        got = core.evaluate(MANIFEST, facts, {}, [], CONFIG)
        self.assertEqual(got, {
            "name": "loader",
            "complete": False,
            "next_tick_seconds": core.TICK_WAITING_ON_PEOPLE,
            "nodes": [
                {"id": "infra", "kind": "pr", "ref": "chainguard-dev/data-analytics-infra#21", "title": "infra", "url": "u21",
                 "phase": "actionable", "checks": "green", "failing": [], "review": "REVIEW_REQUIRED",
                 "waiting_on": [{"node": "bootstrap", "condition": "done", "unknown": False}],
                 "actions": [{"kind": "request-review", "auto": False, "detail": {
                     "channels": [{"channel": "#chan-data", "why": "team chainguard-dev/data-analytics-admins"}],
                     "message": ":pr: [infra](u21)"}}]},
                {"id": "bootstrap", "kind": "step", "title": "Owner applies bootstrap/", "phase": "actionable",
                 "waiting_on": [], "actions": [{"kind": "do-step", "auto": False, "detail": "jml"}]},
                {"id": "seed", "kind": "step", "title": "Dispatch the seed", "phase": "blocked",
                 "waiting_on": [{"node": "infra", "condition": "deployed:prod", "unknown": False}], "actions": []},
                {"id": "models", "kind": "pr", "ref": "chainguard-dev/internal-analytics#517", "title": "models", "url": "u517",
                 "phase": "actionable", "checks": "failing", "failing": ["dbt"], "review": "REVIEW_REQUIRED",
                 "waiting_on": [{"node": "seed", "condition": "done", "unknown": False}],
                 "actions": [{"kind": "check-failures", "auto": False,
                              "detail": {"failing": ["dbt"], "note": "dependencies not landed; failures may be expected"}}]},
            ],
        })

    def test_merged_awaiting_deploy_then_review_already_requested(self):
        facts = {
            "prs": {
                "infra": pr(state="MERGED", merge_commit="abc"),
                "models": pr(url="u517", title="models"),
            },
            "deploys": {"infra": {"prod": None}},
            "checks": {},
        }
        requests = [{"rollout": "loader", "pr": "chainguard-dev/internal-analytics#517", "channel": "#x", "sent_at": "2026-11-18T10:00:00+00:00"}]
        got = core.evaluate(MANIFEST, facts, {"bootstrap": "2026-11-18T09:00:00+00:00"}, requests, CONFIG)
        phases = {n["id"]: (n["phase"], n["actions"], n["waiting_on"]) for n in got["nodes"]}
        self.assertEqual(phases, {
            "infra": ("deploying", [], []),
            "bootstrap": ("done", [], []),
            "seed": ("blocked", [], [{"node": "infra", "condition": "deployed:prod", "unknown": True}]),
            "models": ("blocked", [], [{"node": "seed", "condition": "done", "unknown": False}]),
        })
        self.assertEqual(got["next_tick_seconds"], core.TICK_DEPLOYING)

    def test_no_review_request_while_checks_run(self):
        manifest = {"name": "m", "nodes": [{"id": "a", "pr": "o/r#1"}]}
        running = pr(checks=[{"name": "dbt", "conclusion": None, "status": "IN_PROGRESS"}])
        got = core.evaluate(manifest, {"prs": {"a": running}, "deploys": {}, "checks": {}}, {}, [], CONFIG)
        node = got["nodes"][0]
        self.assertEqual((node["checks"], node["actions"], node["phase"]), ("pending", [], "waiting"))
        self.assertEqual(got["next_tick_seconds"], core.TICK_CHECKS_RUNNING)

    def test_review_gated_check_waits_on_people(self):
        manifest = {"name": "m", "nodes": [{"id": "a", "pr": "o/r#1"}]}
        config = CONFIG | {"review_gated_checks": ["doc-approval"]}
        checks = [{"name": "ci", "conclusion": "SUCCESS", "status": "COMPLETED"},
                  {"name": "doc-approval", "conclusion": None, "status": "IN_PROGRESS"}]
        requests = [{"rollout": "m", "pr": "o/r#1", "channel": "#x", "sent_at": "2026-11-18T10:00:00+00:00"}]
        got = core.evaluate(manifest, {"prs": {"a": pr(checks=checks)}, "deploys": {}, "checks": {}}, {}, requests, config)
        node = got["nodes"][0]
        self.assertEqual((node["checks"], node["failing"], node["awaiting_review"], node["actions"], node["phase"]),
                         ("green", [], ["doc-approval"], [], "waiting"))
        self.assertEqual(got["next_tick_seconds"], core.TICK_WAITING_ON_PEOPLE)

    def test_review_gated_failure_is_not_a_ci_failure(self):
        manifest = {"name": "m", "nodes": [{"id": "a", "pr": "o/r#1"}]}
        config = CONFIG | {"review_gated_checks": ["doc-approval"]}
        checks = [{"name": "doc-approval", "conclusion": "FAILURE", "status": "COMPLETED"}]
        got = core.evaluate(manifest, {"prs": {"a": pr(checks=checks)}, "deploys": {}, "checks": {}}, {}, [], config)
        self.assertEqual([a["kind"] for a in got["nodes"][0]["actions"]], ["request-review"])

    def test_approved_waits_for_review_gated_checks_before_merge(self):
        manifest = {"name": "m", "nodes": [{"id": "a", "pr": "o/r#1"}]}
        config = CONFIG | {"review_gated_checks": ["doc-approval"]}
        checks = [{"name": "doc-approval", "conclusion": None, "status": "IN_PROGRESS"}]
        got = core.evaluate(manifest, {"prs": {"a": pr(checks=checks, review_decision="APPROVED")}, "deploys": {}, "checks": {}},
                            {}, [], config)
        self.assertEqual((got["nodes"][0]["actions"], got["next_tick_seconds"]), ([], core.TICK_CHECKS_RUNNING))

    def test_approved_green_and_landed_dependencies_offers_merge(self):
        manifest = {"name": "m", "nodes": [{"id": "a", "pr": "o/r#1"}, {"id": "b", "pr": "o/r#2", "after": ["a"]}]}
        facts = {"prs": {"a": pr(state="MERGED"), "b": pr(review_decision="APPROVED", mergeable="CONFLICTING")}, "deploys": {}, "checks": {}}
        got = core.evaluate(manifest, facts, {}, [], CONFIG)
        self.assertEqual(got["nodes"][1]["actions"], [{"kind": "rebase", "auto": True, "detail": "push now"}])

    def test_validate(self):
        bad = {"name": "x", "nodes": [{"id": "a", "pr": "o/r#1", "after": ["b"]}, {"id": "b", "step": "s", "after": ["a@landed"]}]}
        self.assertEqual(core.validate(bad), ["b: unknown condition 'landed'"])
        cyc = {"name": "x", "nodes": [{"id": "a", "pr": "o/r#1", "after": ["b"]}, {"id": "b", "step": "s", "after": ["a"]}]}
        self.assertEqual(core.validate(cyc), ["after edges form a cycle"])


class ChaseTest(unittest.TestCase):
    def test_overdue_skips_answered_and_closed(self):
        requests = [
            {"rollout": "r", "pr": "o/r#1", "channel": "#a", "sent_at": "2026-11-16T09:00:00+00:00"},  # Monday
            {"rollout": "r", "pr": "o/r#2", "channel": "#a", "sent_at": "2026-11-16T09:00:00+00:00"},
            {"rollout": "r", "pr": "o/r#3", "channel": "#a", "sent_at": "2026-11-16T09:00:00+00:00"},
            {"rollout": "r", "pr": "o/r#4", "channel": "#a", "sent_at": "2026-11-18T09:00:00+00:00"},
            {"rollout": "r", "pr": "o/r#5", "channel": "#a", "sent_at": "2026-11-17T09:00:00+00:00"},
            {"rollout": "r", "pr": "o/r#5", "channel": "DM rev", "sent_at": "2026-11-17T10:00:00+00:00"},
        ]
        prs = {
            "o/r#1": pr(url="u1", title="one", requested_teams=["o/t"]),
            "o/r#2": pr(activity=[{"author": "rev", "at": "2026-11-16T12:00:00+00:00"}]),
            "o/r#3": pr(state="MERGED"),
            "o/r#4": pr(),
            "o/r#5": pr(url="u5", title="five", activity=[{"author": "chainguard", "at": "2026-11-17T12:00:00+00:00"}]),
        }
        now = datetime(2026, 11, 18, 10, 0, tzinfo=timezone.utc)
        self.assertEqual(core.chase(requests, prs, now, CONFIG), [{
            "pr": "o/r#1", "title": "one", "url": "u1", "rollout": "r", "channel": "#a", "channels_tried": ["#a"], "permalink": None,
            "sent_at": "2026-11-16T09:00:00+00:00", "weekday_hours_waited": 49.0, "threshold_hours": 4.0,
            "requested_teams": ["o/t"], "requested_users": [],
        }, {
            "pr": "o/r#5", "title": "five", "url": "u5", "rollout": "r", "channel": "DM rev", "channels_tried": ["#a", "DM rev"], "permalink": None,
            "sent_at": "2026-11-17T10:00:00+00:00", "weekday_hours_waited": 24.0, "threshold_hours": 4.0,
            "requested_teams": [], "requested_users": [],
        }])

    def test_weekend_does_not_count(self):
        fri = datetime(2026, 11, 20, 22, 0, tzinfo=timezone.utc)
        mon = datetime(2026, 11, 23, 2, 0, tzinfo=timezone.utc)
        self.assertEqual(core.weekday_hours(fri, mon), 4.0)  # Fri 22-24, Mon 00-02


if __name__ == "__main__":
    unittest.main()
