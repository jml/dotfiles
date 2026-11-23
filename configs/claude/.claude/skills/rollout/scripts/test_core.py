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
                 "actions": [{"kind": "request-review", "auto": True, "detail": {
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

    def test_someone_elses_pr_is_only_waited_for(self):
        manifest = {"name": "m", "nodes": [{"id": "a", "pr": "o/r#1"}, {"id": "b", "pr": "o/r#2"}]}
        config = CONFIG | {"me": "jml"}
        threads = [{"resolved": False, "last_author": "reviewer", "started_by_bot": False}]
        facts = {"prs": {"a": pr(number=1, author="robert", threads=threads, mergeable="CONFLICTING",
                                 checks=[{"name": "ci", "conclusion": "FAILURE", "status": "COMPLETED"}]),
                         "b": pr(number=2, author="robert", review_decision="APPROVED")},
                 "deploys": {}, "checks": {}}
        got = core.evaluate(manifest, facts, {}, [], config)
        self.assertEqual([(n["phase"], n["actions"], n["author"]) for n in got["nodes"]],
                         [("waiting", [], "robert"), ("waiting", [], "robert")])
        self.assertEqual(got["next_tick_seconds"], core.TICK_WAITING_ON_PEOPLE)

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


class BotReviewGateTest(unittest.TestCase):
    """A person is asked to review only once the review bots are finished."""

    MANIFEST = {"name": "m", "nodes": [{"id": "a", "pr": "o/r#1"}]}
    CI = {"name": "ci", "conclusion": "SUCCESS", "status": "COMPLETED"}

    def run_case(self, checks=(), threads=()):
        facts = {"prs": {"a": pr(url="u", title="t", checks=[self.CI, *checks], threads=list(threads))}, "deploys": {}, "checks": {}}
        n = core.evaluate(self.MANIFEST, facts, {}, [], CONFIG)["nodes"][0]
        return n.get("bot_review"), [a["kind"] for a in n["actions"]]

    def test_states(self):
        running = {"name": "reviewer/developer-platform", "conclusion": None, "status": "IN_PROGRESS"}
        neutral = {"name": "reviewer/developer-platform", "conclusion": "NEUTRAL", "status": "COMPLETED"}
        gate_ok = {"name": "Checks review gate", "conclusion": "SUCCESS", "status": "COMPLETED"}
        gate_no = {"name": "Checks review gate", "conclusion": "NEUTRAL", "status": "COMPLETED"}
        skipped = {"name": "claude-code-action", "conclusion": "SKIPPED", "status": "COMPLETED"}
        bot_open = {"resolved": False, "last_author": "chainguard", "started_by_bot": True}
        bot_done = {"resolved": True, "last_author": "jml", "started_by_bot": True}
        person_open = {"resolved": False, "last_author": "rev", "started_by_bot": False}
        cases = {
            "no bots": ((skipped,), (), (None, ["request-review"])),
            "bot review running": ((running,), (), ("pending", [])),
            "bot finding open": ((neutral, gate_no), (bot_open,), ("findings", ["respond"])),
            "gate passed": ((neutral, gate_ok), (bot_done,), ("approved", [])),
            "bots done, no approval": ((neutral, gate_no), (bot_done,), ("done", ["request-review"])),
            "person thread only": ((neutral, gate_no), (person_open,), ("done", ["respond", "request-review"])),
        }
        got = {name: self.run_case(c, t) for name, (c, t, _) in cases.items()}
        self.assertEqual(got, {name: want for name, (_, _, want) in cases.items()})

    def test_bot_activity_is_not_an_answer(self):
        requests = [{"rollout": "r", "pr": "o/r#1", "channel": "#a", "sent_at": "2026-11-16T09:00:00+00:00"}]
        prs = {"o/r#1": pr(activity=[{"author": "some-app", "at": "2026-11-16T12:00:00+00:00", "bot": True}])}
        now = datetime(2026, 11, 18, 10, 0, tzinfo=timezone.utc)
        self.assertEqual([o["pr"] for o in core.chase(requests, prs, now, CONFIG)], ["o/r#1"])


class CiSettledTest(unittest.TestCase):
    def test_settled(self):
        config = CONFIG | {"review_gated_checks": ["doc-approval"]}
        done = {"name": "ci", "conclusion": "SUCCESS", "status": "COMPLETED"}
        running = {"name": "ci", "conclusion": None, "status": "IN_PROGRESS"}
        gated = {"name": "doc-approval", "conclusion": None, "status": "IN_PROGRESS"}
        bot = {"name": "reviewer/developer-platform", "conclusion": None, "status": "IN_PROGRESS"}
        cases = {
            "ci running": (pr(checks=[running]), False),
            "ci done, approval-gated check open": (pr(checks=[done, gated]), True),
            "ci done, bot reviewing": (pr(checks=[done, bot]), False),
            "ci failed": (pr(checks=[{"name": "ci", "conclusion": "FAILURE", "status": "COMPLETED"}]), True),
            "merged while running": (pr(state="MERGED", checks=[running]), True),
        }
        self.assertEqual({k: core.ci_settled(p, config) for k, (p, _) in cases.items()},
                         {k: want for k, (_, want) in cases.items()})


class EodTest(unittest.TestCase):
    def test_buckets(self):
        now = datetime(2026, 11, 23, 18, tzinfo=timezone.utc)  # a Monday
        first_run = datetime(2026, 11, 20, tzinfo=timezone.utc)
        green, running = pr()["checks"], [{"name": "ci", "conclusion": None, "status": "IN_PROGRESS"}]

        def ref_pr(n, **kw):
            return pr(**({"number": n, "url": f"u{n}", "title": f"t{n}", "created_at": "2026-11-23T09:00:00Z",
                          "last_commit_at": "2026-11-23T09:00:00Z"} | kw))

        prs = {
            "chainguard-dev/mono#1": ref_pr(1, draft=True),
            "chainguard-dev/mono#2": ref_pr(2, threads=[{"resolved": False, "last_author": "reviewer", "started_by_bot": False}]),
            "chainguard-dev/mono#3": ref_pr(3, checks=[{"name": "ci", "conclusion": "FAILURE", "status": "COMPLETED"}] * 2),
            "chainguard-dev/mono#4": ref_pr(4, files=["public/go-driftlessaf/x.go"], last_commit_at="2026-11-23T08:00:00Z"),
            "chainguard-dev/mono#5": ref_pr(5, draft=True, created_at="2026-11-01T00:00:00Z", last_commit_at="2026-11-02T00:00:00Z"),
            "chainguard-dev/mono#6": ref_pr(6, draft=True),
            "chainguard-dev/mono#7": ref_pr(7),
            "chainguard-dev/mono#8": ref_pr(8, additions=4000),
            "chainguard-dev/mono#9": ref_pr(9, checks=running),
        }
        holds = {"chainguard-dev/mono#1": {"until": "2026-11-30", "reason": "on purpose"}}
        requests = [
            {"rollout": "eod", "pr": "chainguard-dev/mono#7", "channel": "#c", "sent_at": "2026-11-23T15:00:00+00:00"},
            {"rollout": "eod", "pr": "chainguard-dev/mono#8", "channel": "#c", "sent_at": "2026-11-20T09:00:00+00:00"},
        ]
        got = core.eod(prs, requests, holds, now, first_run, CONFIG)
        overdue = core.chase(requests, prs, now, CONFIG)[0]

        def row(n, bucket, detail=None, actions=(), draft=False, stale=False, active="2026-11-23T09:00:00+00:00"):
            return {"ref": f"chainguard-dev/mono#{n}", "title": f"t{n}", "url": f"u{n}", "draft": draft,
                    "bucket": bucket, "detail": detail, "stale": stale, "last_activity": active, "actions": list(actions)}

        ask = {"channels": [{"channel": "#chan-lib", "why": "path public/go-driftlessaf/"}], "message": ":pr: [t4](u4)"}
        self.assertEqual(got, {
            "prs": [
                row(1, "held", holds["chainguard-dev/mono#1"], draft=True),
                row(2, "reply", {"person_threads": 1, "review": "REVIEW_REQUIRED"}),
                row(3, "fix", {"failing": ["ci"], "conflicting": False, "bot_findings": 0}),
                row(4, "ask", ask, [{"kind": "request-review", "auto": True, "detail": ask}], active="2026-11-23T08:00:00+00:00"),
                # Opened before the first /eod: may be a draft on purpose, so jml is asked.
                row(5, "ready", None, [{"kind": "mark-ready", "auto": False, "detail": "draft is green"}],
                    draft=True, stale=True, active="2026-11-02T00:00:00+00:00"),
                row(6, "ready", None, [{"kind": "mark-ready", "auto": True, "detail": "draft is green"}], draft=True),
                row(7, "waiting", {"channel": "#c", "sent_at": "2026-11-23T15:00:00+00:00", "teams": []}),
                row(8, "overdue", overdue),
                row(9, "running"),
            ],
            "counts": {"held": 1, "reply": 1, "fix": 1, "merge": 0, "ask": 1, "ready": 2, "bots": 0,
                       "overdue": 1, "waiting": 1, "running": 1, "parked": 0},
            "oldest_ask_hours": 10.0,
        })
        self.assertEqual(overdue["pr"], "chainguard-dev/mono#8")


if __name__ == "__main__":
    unittest.main()
