# Fixtures

Each fixture is a piece of agent-drafted text with known problems, the findings
a review must report, and the things a review must leave alone. Use them to
spot-check the `reviewing-prose` skill after changing a rule.

To run a check, copy each fixture up to its **Must find** heading into a
separate directory, so that the reviewer can't see the expected findings. Run
the skill on each copy, and compare its report with **Must find** and **Must not
flag**.

The "must not flag" lists matter as much as the findings. A rule applied where
its exception holds makes the text worse, and that failure is easy to miss.

The texts are invented, and none of them appears in the casebook, so a reviewer
that loads the casebook hasn't seen the answers. Each fixture names the casebook
cases whose failures it repeats, under **Source** at the end, after the
expected findings.
