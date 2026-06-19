#!/usr/bin/env bash
# PreToolUse (Bash) hook: enforce the Emacs-reviewed /commit workflow.
#
# Commits must pass through the blocking Emacs/magit review gate that the
# /commit and /pr slash commands open, not land via a raw `git commit`.
# This hook denies any Bash tool call that creates or amends a commit unless
# it carries the review-gate marker GIT_EDITOR=emacsclient that those commands
# set. It fails open: malformed or empty input is allowed through.

input=$(cat)
cmd=$(printf '%s' "$input" | jq -r '.tool_input.command // empty' 2>/dev/null)

# Nothing to inspect -> allow.
[ -n "$cmd" ] || exit 0

# Allow the review-gated path: /commit and /pr invoke git with this editor.
case "$cmd" in
  *GIT_EDITOR=emacsclient*) exit 0 ;;
esac

# Match the porcelain `git commit` as a standalone word, so that
# `git commit-tree`, `git commit-graph`, and names like `mygit` do not trip
# the gate. The boundaries also catch compound commands (`cd x && git commit`).
re='(^|[^[:alnum:]_-])git[[:space:]]+commit([[:space:]]|$|[;&|)])'
if [[ "$cmd" =~ $re ]]; then
  reason="Direct 'git commit' is blocked here. Land commits through the /commit command (or /pr), which opens the message in Emacs/magit for review before anything is committed. If a raw commit is genuinely needed, ask the user to run it themselves."
  jq -n --arg r "$reason" \
    '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"deny",permissionDecisionReason:$r}}'
fi

exit 0
