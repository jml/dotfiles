"""Tests for page.render: the page shows what needs jml, every node, and its edges."""

import re
import unittest
from datetime import datetime, timezone

import core
import page
from test_core import CONFIG, MANIFEST, pr


class PageTest(unittest.TestCase):
    def test_render(self):
        facts = {
            "prs": {
                "infra": pr(repo="chainguard-dev/data-analytics-infra", url="https://github.com/chainguard-dev/data-analytics-infra/pull/21",
                            title="infra", requested_teams=["chainguard-dev/data-analytics-admins"]),
                "models": pr(repo="chainguard-dev/internal-analytics", url="u517", title="models"),
            },
            "deploys": {"infra": {"prod": False}},
            "checks": {},
        }
        report = core.evaluate(MANIFEST, facts, {}, [], CONFIG)
        html = page.render(MANIFEST, report, [], datetime(2026, 11, 18, 19, 0, tzinfo=timezone.utc))
        self.assertIn("<title>loader rollout</title>", html)
        # The review request jml must approve, with its exact message and channel.
        self.assertIn("Approve the review request", html)
        self.assertIn("<code>:pr: [infra](https://github.com/chainguard-dev/data-analytics-infra/pull/21)</code>", html)
        self.assertIn("to #chan-data <span", html)
        # A PR that can be reviewed before its dependencies land says what its merge waits for.
        self.assertIn("<td>merges after bootstrap</td>", html)
        # One box per node, and the deploy condition labelled on its edge.
        self.assertEqual(len(re.findall(r'<rect class="node ', html)), len(MANIFEST["nodes"]))
        self.assertIn(">deployed:prod</text>", html)
        self.assertEqual(page.display_state(report["nodes"][2]), "blocked")

    def test_layout_layers_follow_edges(self):
        xy = page.layout(MANIFEST)
        self.assertLess(xy["bootstrap"][0], xy["infra"][0])
        self.assertLess(xy["infra"][0], xy["seed"][0])
        self.assertLess(xy["seed"][0], xy["models"][0])


class NewWorkAndOwnershipTest(unittest.TestCase):
    MANIFEST = {"name": "r", "nodes": [
        {"id": "fix", "work": "Add workflow-rules to the ruleset", "proposed": "found while checking the first run's prerequisites"},
        {"id": "infra", "work": "Grant D&A access in github-iac", "session": "local_abc"},
        {"id": "owner-step", "step": "A project Owner applies bootstrap/", "by": "external"},
        {"id": "models", "pr": "o/r#1", "after": ["fix", "infra"]},
    ], "follow_ups": [{"ref": "DEV-1", "url": "https://linear.app/x/DEV-1", "title": "Tidy the loader's alerts"}]}

    def report(self):
        facts = {"prs": {"models": pr(url="u1", title="models")}, "deploys": {}, "checks": {}}
        return core.evaluate(self.MANIFEST, facts, {}, [], CONFIG)

    def test_core(self):
        got = {n["id"]: (n["phase"], [(a["kind"], core.whose_move(a)) for a in n["actions"]], [w["node"] for w in n["waiting_on"]])
               for n in self.report()["nodes"]}
        self.assertEqual(got, {
            "fix": ("proposed", [("decide-scope", "jml")], []),
            "infra": ("building", [], []),
            "owner-step": ("actionable", [("do-step", "other")], []),
            "models": ("actionable", [("request-review", "jml")], ["fix", "infra"]),
        })

    def test_page_sections(self):
        html = page.render(self.MANIFEST, self.report(), [], datetime(2026, 11, 18, 19, 0, tzinfo=timezone.utc))
        yours = html.split('<h2 id="yours">')[1].split("</section>")[0]
        others = html.split('<h2 id="others">')[1].split("</section>")[0]
        self.assertIn("Decide whether this is part of the rollout", yours)
        self.assertNotIn("bootstrap", yours)
        self.assertIn("Someone else does this step", others)
        self.assertIn("in session local_abc", html)
        self.assertIn("Tidy the loader", html.split('<h2 id="fu">')[1])


class RegressionTest(unittest.TestCase):
    def test_merged_pr_with_request_is_not_waiting_on_review(self):
        manifest = {"name": "r", "nodes": [{"id": "grant", "pr": "o/r#1"}, {"id": "use", "step": "Set the variable", "after": ["grant@deployed:prod"]}]}
        facts = {"prs": {"grant": pr(state="MERGED", url="u1", title="grant")}, "deploys": {"grant": {"prod": False}}, "checks": {}}
        requests = [{"rollout": "r", "pr": "o/r#1", "channel": "#a", "sent_at": "2026-11-18T10:00:00+00:00"}]
        report = core.evaluate(manifest, facts, {}, requests, CONFIG)
        html = page.render(manifest, report, requests, datetime(2026, 11, 18, 19, 0, tzinfo=timezone.utc))
        self.assertNotIn("Waiting on review", html)

    def test_every_theme_block_defines_every_colour_token(self):
        html = page.render(MANIFEST, core.evaluate(MANIFEST, {"prs": {"infra": pr(), "models": pr()}, "deploys": {}, "checks": {}}, {}, [], CONFIG), [],
                           datetime(2026, 11, 18, 19, 0, tzinfo=timezone.utc))
        css = html.split("<style>")[1].split("</style>")[0]
        light = css.split(":root {")[1].split("}")[0]
        media_dark = css.split(':root:not([data-theme="light"]) {')[1].split("}")[0]
        toggled_dark = css.split(':root[data-theme="dark"] {')[1].split("}")[0]
        colours = lambda block: {t for t in re.findall(r"(--[a-z-]+):\s*#", block)}
        self.assertEqual(colours(media_dark), colours(light))
        self.assertEqual(colours(toggled_dark), colours(light))


if __name__ == "__main__":
    unittest.main()
