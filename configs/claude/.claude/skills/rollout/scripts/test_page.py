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


if __name__ == "__main__":
    unittest.main()
