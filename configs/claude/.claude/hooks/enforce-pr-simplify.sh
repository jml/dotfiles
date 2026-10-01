#!/usr/bin/env bash
# PreToolUse (Bash) hook: enforce the /pr workflow, which runs /simplify on
# PRs over 200 changed lines.
#
# PRs must be opened through /pr, so that large ones pass through /simplify
# before review. /pr opens the PR with a CLAUDE_PR_GATE marker inline:
# `simplified` after a /simplify pass, `small` when the size check skipped it.
# This hook denies any Bash tool call that runs `gh pr create` without one. It fails open: malformed or empty input is allowed through.

input=$(cat)
cmd=$(printf '%s' "$input" | jq -r '.tool_input.command // empty' 2>/dev/null)

# Nothing to inspect -> allow.
[ -n "$cmd" ] || exit 0

# Allow the gated path: /pr sets one of these markers in its simplify step.
case "$cmd" in
  *CLAUDE_PR_GATE=simplified*|*CLAUDE_PR_GATE=small*) exit 0 ;;
esac

# Match `gh pr create` as standalone words, including inside compound commands
# (`cd x && gh pr create`), without tripping on names like `mygh`.
re='(^|[^[:alnum:]_-])gh[[:space:]]+pr[[:space:]]+create([[:space:]]|$|[;&|)])'
if [[ "$cmd" =~ $re ]]; then
  reason="Direct 'gh pr create' is blocked here. Open PRs through the /pr command, which runs /simplify on large changes before posting them for review. If a raw PR is genuinely needed, ask the user to run it themselves."
  jq -n --arg r "$reason" \
    '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"deny",permissionDecisionReason:$r}}'
fi

exit 0
