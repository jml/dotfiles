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

    def test_waiting_on_another_pr_holds_a_draft_and_drafts_a_ready_pr(self):
        manifest = {"name": "m", "nodes": [
            {"id": "a", "pr": "o/r#1"},
            {"id": "b", "pr": "o/r#2", "after": ["a"]},
            {"id": "c", "pr": "o/r#3", "after": ["a@deployed:prod"]},
            {"id": "d", "pr": "o/r#4", "after": ["a"]},
        ]}
        facts = {"prs": {"a": pr(number=1), "b": pr(number=2), "c": pr(number=3), "d": pr(number=4, draft=True)},
                 "deploys": {"a": {"prod": False}}, "checks": {}}
        requests = [{"rollout": "m", "pr": f"o/r#{n}", "channel": "#x", "sent_at": "2026-11-18T10:00:00+00:00"} for n in (1, 2, 3)]
        got = core.evaluate(manifest, facts, {}, requests, CONFIG)
        self.assertEqual({n["id"]: (n["phase"], n["actions"]) for n in got["nodes"]}, {
            "a": ("waiting", []),
            "b": ("actionable", [{"kind": "mark-draft", "auto": True, "detail": ["a merged"]}]),
            "c": ("actionable", [{"kind": "mark-draft", "auto": True, "detail": ["a deployed:prod"]}]),
            # A green draft is not marked ready while what it builds on is unlanded.
            "d": ("blocked", []),
        })

    def test_validate(self):
        bad = {"name": "x", "nodes": [{"id": "a", "pr": "o/r#1", "after": ["b"]}, {"id": "b", "step": "s", "after": ["a@landed"]}]}
        self.assertEqual(core.validate(bad), ["b: unknown condition 'landed'"])
        cyc = {"name": "x", "nodes": [{"id": "a", "pr": "o/r#1", "after": ["b"]}, {"id": "b", "step": "s", "after": ["a"]}]}
        self.assertEqual(core.validate(cyc), ["after edges form a cycle"])


class StageDeployTest(unittest.TestCase):
    def test_dispatched_stage(self):
        for title, want in [
            ("Provision enforce.dev: 400-merge-bot @ abc123", ("400-merge-bot", "abc123")),
            ("Provision chainops.dev: 400-vpa-bot", ("400-vpa-bot", "head")),
            ("Provision enforce.dev:  @ abc123", None),
            ("something else", None),
        ]:
            with self.subTest(title=title):
                self.assertEqual(core.dispatched_stage(title, "head"), want)

    def test_job_deployed_stage(self):
        jobs = [
            {"name": "sharded-stages (400-merge-bot) / Terraform", "conclusion": "success"},
            {"name": "sharded-stages (400-merge-bot-extra) / Terraform", "conclusion": "failure"},
            {"name": "sharded-stages (400-vpa-bot) / Terraform", "conclusion": "failure"},
        ]
        self.assertTrue(core.job_deployed_stage(jobs, "400-merge-bot"))
        self.assertFalse(core.job_deployed_stage(jobs, "400-vpa-bot"))
        self.assertFalse(core.job_deployed_stage(jobs, "400-merge"))


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
            "counts": {"held": 1, "reply": 1, "fix": 1, "stacked": 0, "merge": 0, "owned": 0, "ask": 1, "blocked": 0, "ready": 2, "bots": 0,
                       "overdue": 1, "waiting": 1, "running": 1, "parked": 0},
            "oldest_ask_hours": 10.0,
        })
        self.assertEqual(overdue["pr"], "chainguard-dev/mono#8")


    def test_drafts_that_cannot_merge_now_are_not_offered(self):
        now = datetime(2026, 11, 23, 18, tzinfo=timezone.utc)

        def c(oid, headline):
            return {"oid": oid, "headline": headline}

        def draft(n, **kw):
            return pr(**({"number": n, "url": f"u{n}", "title": f"t{n}", "draft": True, "created_at": "2026-11-23T09:00:00Z",
                          "last_commit_at": "2026-11-23T09:00:00Z", "base": "main", "head": f"b{n}",
                          "default_branch": "main", "commits": [c(f"o{n}", f"h{n}")]} | kw))

        prs = {
            "chainguard-dev/mono#10": draft(10, commits=[c("a", "parent one"), c("b", "parent two")]),
            # Rebased on main after #10 changed: #10's commits survive by headline only.
            "chainguard-dev/mono#11": draft(11, commits=[c("a2", "parent one"), c("b2", "parent two"), c("x", "child")]),
            # A sibling off #10's first commit: it lacks "parent two", so it doesn't build on #10.
            "chainguard-dev/mono#12": draft(12, commits=[c("a", "parent one"), c("y", "sibling")]),
            "chainguard-dev/mono#13": draft(13, base="b10"),
            "chainguard-dev/mono#14": draft(14, body="## To-do before merge\n\n- [x] done\n- [ ] Merge #10 first\n\n## Notes\n- [ ] not a to-do"),
            "chainguard-dev/mono#15": draft(15),
            "chainguard-dev/mono#16": draft(16),
            "chainguard-dev/other#10": draft(10, repo="chainguard-dev/other", url="o10"),
            "chainguard-dev/mono#17": draft(17, draft=False, head="b15"),
        }
        sessions = [
            {"name": "Dataplane guide", "cwd": "/w/a", "branch": "b15"},
            {"name": "Iterate on #16", "cwd": "/w/b", "branch": "main"},
            # mono#10 and other#10 are both open, so "#10" names neither.
            {"name": "Look at #10", "cwd": "/w/c", "branch": "main"},
        ]
        got = core.eod(prs, [], {}, now, now, CONFIG, sessions=sessions)
        self.assertEqual({e["ref"]: (e["bucket"], e["detail"]) for e in got["prs"]}, {
            "chainguard-dev/mono#10": ("ready", None),
            "chainguard-dev/mono#11": ("blocked", {"builds_on": ["chainguard-dev/mono#10"]}),
            "chainguard-dev/mono#12": ("ready", None),
            "chainguard-dev/mono#13": ("blocked", {"builds_on": ["chainguard-dev/mono#10"], "base": "b10"}),
            "chainguard-dev/mono#14": ("blocked", {"todos": ["Merge #10 first"]}),
            "chainguard-dev/mono#15": ("blocked", {"session": "Dataplane guide"}),
            "chainguard-dev/mono#16": ("blocked", {"session": "Iterate on #16"}),
            "chainguard-dev/other#10": ("ready", None),
            # Ready for review, but its live session sends the request.
            "chainguard-dev/mono#17": ("owned", {"session": "Dataplane guide"}),
        })
        self.assertEqual([e["actions"] for e in got["prs"] if e["bucket"] == "blocked"], [[]] * 5)

    def test_a_ready_pr_stacked_on_another_goes_back_to_draft(self):
        now = datetime(2026, 11, 23, 18, tzinfo=timezone.utc)

        def ready(n, **kw):
            return pr(**({"number": n, "url": f"u{n}", "title": f"t{n}", "created_at": "2026-11-23T09:00:00Z",
                          "last_commit_at": "2026-11-23T09:00:00Z", "base": "main", "head": f"b{n}",
                          "default_branch": "main", "commits": [{"oid": f"o{n}", "headline": f"h{n}"}]} | kw))

        prs = {"chainguard-dev/mono#20": ready(20), "chainguard-dev/mono#21": ready(21, base="b20")}
        requests = [{"rollout": "eod", "pr": "chainguard-dev/mono#21", "channel": "#c", "sent_at": "2026-11-23T15:00:00+00:00"}]
        got = core.eod(prs, requests, {}, now, now, CONFIG)
        stack = {"builds_on": ["chainguard-dev/mono#20"], "base": "b20"}
        self.assertEqual({e["ref"]: (e["bucket"], e["detail"], e["actions"]) for e in got["prs"]}, {
            "chainguard-dev/mono#20": ("ask", {"channels": [], "message": ":pr: [t20](u20)"},
                                      [{"kind": "request-review", "auto": False, "detail": {"channels": [], "message": ":pr: [t20](u20)"}}]),
            "chainguard-dev/mono#21": ("blocked", stack, [{"kind": "mark-draft", "auto": True, "detail": stack}]),
        })


class DesignDocStackTest(unittest.TestCase):
    def test_pr_hygiene_waits_on_its_design_doc(self):
        now = datetime(2026, 11, 23, 18, tzinfo=timezone.utc)
        hygiene = {"name": "pr-hygiene", "conclusion": "FAILURE", "status": "COMPLETED"}
        dbt = {"name": "dbt", "conclusion": "FAILURE", "status": "COMPLETED"}
        body = "Adds the models.\n\nDesign-Doc: docs/designs/x.md\n"

        def c(oid, headline):
            return {"oid": oid, "headline": headline}

        def p(n, **kw):
            return pr(**({"number": n, "url": f"u{n}", "title": f"t{n}", "created_at": "2026-11-23T09:00:00Z",
                          "last_commit_at": "2026-11-23T09:00:00Z"} | kw))

        prs = {
            # Its doc commit kept a headline from #2's history, and both merged main.
            "chainguard-dev/mono#1": p(1, files=["docs/designs/x.md"],
                                       commits=[c("m1", "Merge branch 'main'"), c("d1", "more")]),
            "chainguard-dev/mono#2": p(2, body=body, checks=[hygiene], commits=[c("a", "models"), c("b", "more"), c("m2", "Merge branch 'main'")]),
            # Merged in part of #2, so it carries the doc too, but isn't #2's design doc.
            "chainguard-dev/mono#6": p(6, files=["docs/designs/x.md"], commits=[c("a", "models"), c("z", "ladder")]),
            # Built on its design doc #8's branch: #8 still counts.
            "chainguard-dev/mono#7": p(7, body="Design-Doc: docs/designs/y.md", checks=[hygiene], base="b8",
                                       commits=[c("d", "doc"), c("e", "impl")]),
            "chainguard-dev/mono#8": p(8, head="b8", files=["docs/designs/y.md"], commits=[c("d", "doc")]),
            # Another check fails too: that one is jml's to fix.
            "chainguard-dev/mono#3": p(3, body=body, checks=[hygiene, dbt]),
            # No open PR adds its design doc, so pr-hygiene is jml's to fix.
            "chainguard-dev/mono#4": p(4, body="Design-Doc: docs/designs/missing.md", checks=[hygiene]),
            # The same path in another repo isn't its design doc.
            "chainguard-dev/other#5": p(5, repo="chainguard-dev/other", url="o5", body=body, checks=[hygiene]),
        }
        got = core.eod(prs, [], {}, now, now, CONFIG)
        self.assertEqual({e["ref"]: (e["bucket"], e["detail"]) for e in got["prs"] if e["ref"] not in ("chainguard-dev/mono#1", "chainguard-dev/mono#6", "chainguard-dev/mono#8")}, {
            "chainguard-dev/mono#2": ("stacked", {"design_doc": ["chainguard-dev/mono#1"], "failing": ["pr-hygiene"]}),
            "chainguard-dev/mono#3": ("fix", {"failing": ["dbt"], "conflicting": False, "bot_findings": 0}),
            "chainguard-dev/mono#4": ("fix", {"failing": ["pr-hygiene"], "conflicting": False, "bot_findings": 0}),
            # Its branch is #8's, so it is rebased when #8 merges: back to draft.
            "chainguard-dev/mono#7": ("blocked", {"builds_on": ["chainguard-dev/mono#8"]}),
            "chainguard-dev/other#5": ("fix", {"failing": ["pr-hygiene"], "conflicting": False, "bot_findings": 0}),
        })


STANDUP_CONFIG = CONFIG | {
    "channels": CONFIG["channels"] | {"teams": {"chainguard-dev/data-analytics-admins": "#chan-data-eng",
                                                "chainguard-dev/developer-platform-team-write": "#chan-home"}},
    "standup": {
        "linear_name": "Jonathan Lange",
        "subteam": [{"name": "Jonathan Lange", "github": "jml"}, {"name": "Ada Example", "github": "ada"}],
        "pod": [{"name": "Bo Example", "github": "bo"}],
        "subteam_linear_teams": ["DEVFNDN"],
        "home_github_teams": ["chainguard-dev/developer-platform-team-write"],
    },
}


def issue(id_, status="In Progress", status_type="started", assignee="Jonathan Lange", **kw):
    return {"id": id_, "title": f"title {id_}", "url": f"l/{id_}", "status": status, "status_type": status_type,
            "assignee": assignee} | kw


def standup_inputs():
    now = datetime(2026, 11, 24, 14, tzinfo=timezone.utc)

    def ref_pr(n, **kw):
        return pr(**({"number": n, "url": f"u{n}", "title": f"t{n}", "created_at": "2026-11-20T09:00:00Z",
                      "last_commit_at": "2026-11-23T09:00:00Z", "head": f"b{n}"} | kw))

    prs = {
        # Waiting on another team's review, asked in its channel.
        "chainguard-dev/mono#1": ref_pr(1, title="feat: thing (DEV-10)"),
        # Waiting on the home team: a blocker, not a dependency.
        # A description citing DEV-18 as context doesn't link the PR to it.
        "chainguard-dev/mono#2": ref_pr(2, requested_teams=["chainguard-dev/developer-platform-team-write"],
                                        body="Follows [DEV-18].\n\n[DEV-18]: l/DEV-18"),
        # Failing CI, and a draft for DEV-11, which is still Todo.
        "chainguard-dev/mono#3": ref_pr(3, draft=True, head="dev-11-x",
                                        checks=[{"name": "ci", "conclusion": "FAILURE", "status": "COMPLETED"}]),
        # In a rollout: its session owns the request.
        "chainguard-dev/mono#4": ref_pr(4),
        # In a rollout and waiting on review: still a blocker.
        "chainguard-dev/mono#5": ref_pr(5),
        # Stacked on mono#1, which adds its design doc.
        "chainguard-dev/mono#6": ref_pr(6, body="Design-Doc: docs/d.md",
                                        checks=[{"name": "pr-hygiene", "conclusion": "FAILURE", "status": "COMPLETED"}]),
        # A green draft: jml's move, offered for mark-ready. DEV-03 is already
        # in Merged, from an earlier PR.
        "chainguard-dev/mono#7": ref_pr(7, draft=True, head="dev-03-second-part"),
    }
    prs["chainguard-dev/mono#1"]["files"] = ["docs/d.md"]
    requests = [
        {"rollout": "eod", "pr": "chainguard-dev/mono#1", "channel": "#chan-data-eng", "sent_at": "2026-11-24T12:00:00+00:00"},
        {"rollout": "eod", "pr": "chainguard-dev/mono#2", "channel": "#chan-home", "sent_at": "2026-11-24T12:00:00+00:00"},
        {"rollout": "big-change", "pr": "chainguard-dev/mono#5", "channel": "#chan-home", "sent_at": "2026-11-24T12:00:00+00:00"},
    ]
    sweep = core.eod(prs, requests, {}, now, now, STANDUP_CONFIG)
    linear = {"issues": [
        issue("DEV-10", status="In Review", blocked_by=["DATA-1", "DATA-2", "DEV-20"], blocks=["CON-5"]),
        issue("DEV-11", status="Todo", status_type="unstarted"),
        issue("DEVFNDN-12", status="Blocked"),
        issue("DEVFNDN-13", status="Blocked", blocked_by=["DEVFNDN-14"]),
        issue("DEVFNDN-15", blocked_by=["DEVFNDN-16"], blocks=["DEVFNDN-17"]),
        issue("DEV-18"),
        # In Merged with no PR merged since the last standup: still unverified.
        issue("DEV-02", status="Merged"),
        # In Merged with a PR still open: not yet time to check the outcome.
        issue("DEV-03", status="Merged"),
        issue("DATA-1", assignee="Someone Else"),
        # Not started is still open.
        issue("DATA-2", assignee="Someone Else", status="Backlog", status_type="backlog"),
        issue("DEV-20", assignee="Ada Example"),
        issue("CON-5", assignee="Bo Example", status="Todo", status_type="unstarted"),
        issue("DEVFNDN-14", status="Done", status_type="completed"),
        issue("DEVFNDN-16", assignee=None, status="Backlog", status_type="backlog"),
        issue("DEVFNDN-17", assignee="Ada Example"),
    ]}
    merged = [
        {"ref": "chainguard-dev/mono#8", "title": "fix: tidy UTF-8 handling", "url": "u8", "body": "no ticket", "head": "fix-utf"},
        # Cites an issue as context: planned work, not a fix in passing.
        {"ref": "chainguard-dev/mono#10", "title": "docs: note", "url": "u10", "body": "See [DEV-99].", "head": "b10"},
        {"ref": "chainguard-dev/mono#9", "title": "feat: last piece", "url": "u9", "body": "Tracking: DEV-18", "head": "b9"},
    ]
    return sweep, prs, {"chainguard-dev/mono#4": "big-change", "chainguard-dev/mono#5": "big-change"}, linear, merged


class StandupTest(unittest.TestCase):
    def test_first_run_is_a_baseline(self):
        got = core.standup(*standup_inputs(), None, STANDUP_CONFIG)
        self.assertTrue(got["baseline"])
        self.assertEqual(got["counts"], {"blockers": {"new": 0, "standing": 5, "cleared": 0},
                                         "dependencies": {"new": 0, "changed": 0, "standing": 3, "resolved": 0}})
        self.assertEqual([(b["key"], b.get("whose_move"), b.get("rollout"), b.get("holds_back"))
                          for b in got["blockers"]["standing"]], [
            ("issue:DEV-10", None, None, None),
            ("issue:DEVFNDN-12", None, None, None),
            ("pr:chainguard-dev/mono#1", "reviewers", None, ["chainguard-dev/mono#6"]),
            ("pr:chainguard-dev/mono#2", "reviewers", None, []),
            ("pr:chainguard-dev/mono#5", "reviewers", "big-change", []),
            # mono#3 fails CI and mono#4 is a rollout's to ask for: both are someone's
            # move on this side, so neither is a blocker. mono#6 rides on mono#1.
            # DEVFNDN-15 waits only on an unassigned issue in the subteam's team, and
            # DEVFNDN-13's blocker is done: neither is a standup blocker.
        ])
        # Offers leave out the rollout's PRs: their sessions send them.
        self.assertEqual([(o["ref"], [a["kind"] for a in o["actions"]]) for o in got["offers"]],
                         [("chainguard-dev/mono#7", ["mark-ready"])])
        # Only Linear relations are dependencies. mono#1 waits on another team's
        # review, which makes it a blocker, and a review asked of jml is jml's move.
        self.assertEqual([(d["key"], d["counterpart"]["kind"], d["state"]) for d in got["dependencies"]["standing"]], [
            ("waited_on_by|DEV-10|CON-5", "pod", "Todo"),
            ("waits_on|DEV-10|DATA-1", "other", "In Progress"),
            ("waits_on|DEV-10|DATA-2", "other", "Backlog"),
        ])
        self.assertEqual([(d["id"], d["rule"], d["propose"]) for d in got["drift"]], [
            ("DEV-11", "pr-open-issue-not-started", "In Progress"),
            ("DEV-18", "prs-merged", "Merged"),
            ("DEVFNDN-12", "blocked-without-blocker", "add a blocked-by relation to what it waits on, or move it out of Blocked"),
            ("DEVFNDN-13", "blocker-resolved", "move it out of Blocked"),
            # Issues in Merged come last.
            ("DEV-02", "merged-unverified", "check the outcome, then mark it Done"),
        ])
        self.assertEqual(got["fixed_in_passing"], [{"ref": "chainguard-dev/mono#8", "title": "fix: tidy UTF-8 handling", "url": "u8"}])

    def test_compares_with_the_last_standup(self):
        inputs = standup_inputs()
        first = core.standup(*inputs, None, STANDUP_CONFIG)["snapshot"]
        previous = {"at": "2026-11-23T14:00:00+00:00",
                    "blockers": {k: v for k, v in first["blockers"].items() if k != "pr:chainguard-dev/mono#2"}
                    | {"pr:chainguard-dev/mono#99": {"key": "pr:chainguard-dev/mono#99", "title": "gone", "url": "u99"}},
                    "dependencies": {k: (v | {"state": "Todo"} if k == "waits_on|DEV-10|DATA-1" else v)
                                     for k, v in first["dependencies"].items() if k != "waited_on_by|DEV-10|CON-5"}}
        got = core.standup(*inputs, previous, STANDUP_CONFIG)
        self.assertEqual((got["baseline"], got["since"]), (False, "2026-11-23T14:00:00+00:00"))
        self.assertEqual(got["counts"], {"blockers": {"new": 1, "standing": 4, "cleared": 1},
                                         "dependencies": {"new": 1, "changed": 1, "standing": 1, "resolved": 0}})
        self.assertEqual([b["key"] for b in got["blockers"]["new"]], ["pr:chainguard-dev/mono#2"])
        self.assertEqual(got["blockers"]["cleared"], [{"key": "pr:chainguard-dev/mono#99", "title": "gone", "url": "u99"}])
        self.assertEqual([(d["key"], d["was"], d["state"]) for d in got["dependencies"]["changed"]],
                         [("waits_on|DEV-10|DATA-1", "Todo", "In Progress")])
        self.assertEqual([d["key"] for d in got["dependencies"]["new"]], ["waited_on_by|DEV-10|CON-5"])


if __name__ == "__main__":
    unittest.main()
