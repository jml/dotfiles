import unittest
from datetime import datetime, timezone

from waiting import classify

NOW = datetime(2026, 11, 23, tzinfo=timezone.utc)


def item(**kw):
    base = {
        "ref": "o/r#1", "url": "u", "title": "t", "author": "alice", "is_pr": True, "is_draft": False,
        "updated_at": "2026-11-20T00:00:00Z", "review_decision": None, "requested": [], "events": [],
    }
    return base | kw


def ev(author, at, body, url="c"):
    return {"author": author, "at": at, "body": body, "url": url}


class ClassifyTest(unittest.TestCase):
    def test_ask_after_my_last_comment_waits(self):
        got = classify(item(events=[
            ev("jml", "2026-11-10T00:00:00Z", "done"),
            ev("alice", "2026-11-11T00:00:00Z", "@jml please reapprove", "c2"),
        ]), "jml", NOW)
        self.assertEqual(got["kind"], "asked")
        self.assertEqual(got["waiting_since"], "2026-11-11T00:00:00Z")
        self.assertEqual([a["url"] for a in got["asks"]], ["c2"])

    def test_my_reply_clears_the_ask(self):
        got = classify(item(events=[
            ev("alice", "2026-11-11T00:00:00Z", "@jml please reapprove"),
            ev("jml", "2026-11-12T00:00:00Z", "approved"),
        ]), "jml", NOW)
        self.assertIsNone(got)

    def test_longer_login_and_bots_are_not_asks(self):
        got = classify(item(events=[
            ev("alice", "2026-11-11T00:00:00Z", "cc @jmlx"),
            ev("linear", "2026-11-11T00:00:00Z", "@jml linked"),
        ]), "jml", NOW)
        self.assertIsNone(got)

    def test_named_request_without_ask(self):
        got = classify(item(requested=["jml"], updated_at="2026-10-01T00:00:00Z"), "jml", NOW)
        self.assertEqual((got["kind"], got["stale"]), ("review", True))

    def test_draft_needs_a_direct_ask(self):
        self.assertIsNone(classify(item(is_draft=True, requested=["jml"]), "jml", NOW))
        got = classify(item(is_draft=True, requested=["jml"], events=[
            ev("alice", "2026-11-11T00:00:00Z", "@jml please review the direction"),
        ]), "jml", NOW)
        self.assertEqual(got["kind"], "asked")

    def test_bot_pr_request(self):
        for author in ("dependabot[bot]", "app/dependabot"):
            got = classify(item(author=author, requested=["jml"]), "jml", NOW)
            self.assertEqual(got["kind"], "bot-review", author)


if __name__ == "__main__":
    unittest.main()
