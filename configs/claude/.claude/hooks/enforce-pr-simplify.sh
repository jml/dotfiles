#!/usr/bin/env bash
# PreToolUse (Bash) hook: enforce the /pr workflow, which runs /simplify.
#
# Changes posted for review must first pass through /simplify. The /pr slash
# command runs it and then opens the PR with the marker CLAUDE_PR_GATE=simplified
# inline; this hook denies any Bash tool call that runs `gh pr create` without
# that marker. It fails open: malformed or empty input is allowed through.

input=$(cat)
cmd=$(printf '%s' "$input" | jq -r '.tool_input.command // empty' 2>/dev/null)

# Nothing to inspect -> allow.
[ -n "$cmd" ] || exit 0

# Allow the gated path: /pr sets this marker after running /simplify.
case "$cmd" in
  *CLAUDE_PR_GATE=simplified*) exit 0 ;;
esac

# Match `gh pr create` as standalone words, including inside compound commands
# (`cd x && gh pr create`), without tripping on names like `mygh`.
re='(^|[^[:alnum:]_-])gh[[:space:]]+pr[[:space:]]+create([[:space:]]|$|[;&|)])'
if [[ "$cmd" =~ $re ]]; then
  reason="Direct 'gh pr create' is blocked here. Open PRs through the /pr command, which runs /simplify on the change before posting it for review. If a raw PR is genuinely needed, ask the user to run it themselves."
  jq -n --arg r "$reason" \
    '{hookSpecificOutput:{hookEventName:"PreToolUse",permissionDecision:"deny",permissionDecisionReason:$r}}'
fi

exit 0
