#!/bin/bash
# Claude session collector for the /wip skill.
#
# Reports what Claude Code sessions have been doing recently, as pipe-delimited
# sections that join against inventory.sh's == WORKTREES == by path. All times
# are UTC, minute precision.
#
#   == LIVE ==          sessions with a running process:
#                       sid|kind|status|name|cwd|jobId|parkedJobId|source
#                       `claude agents --json --all` omits idle interactive
#                       terminals, and ~/.claude/sessions/*.json keeps entries
#                       for exited processes, so the two are unioned and the
#                       registry is filtered by `kill -0`.
#   == JOBS ==          background jobs updated within the window:
#                       jobId|sid|state|updated|cwd|name|intent|detail
#                       `detail` is the job's own account of its result, not a
#                       verified fact; check it against git and GitHub.
#   == SESSIONS ==      one line per candidate session, newest prompt first:
#                       sid|liveness|first-prompt|last-prompt|prompts=N|
#                       transcript-mtime|size|card=fresh|stale|none|title|
#                       first=<first typed prompt>|last=<last typed prompt>
#   == SESSION PATHS == sid|cwd — every cwd the transcript records. A session
#                       that enters a worktree starts at the repo root and
#                       moves into .claude/worktrees/<name>, so this is a set.
#   == SESSION BRANCHES == sid|branch, likewise a set.
#   == LINEAGE ==       parent|child|reason. `parked`: an interactive session
#                       parked into a background job (the registry records the
#                       job id). `respawned`: a live process serves a job under
#                       a newer session id. `shared-root`: two transcripts begin with the
#                       same user record, which is how --resume, --fork-session
#                       and parking copy a conversation; the smaller transcript
#                       is listed as parent, which is a guess.
#   == WORKTREE SESSIONS == worktree|sid|liveness|last-prompt — the join of
#                       SESSION PATHS against every checkout under BASE.
#
# Candidates are the sessions with a typed prompt in ~/.claude/history.jsonl
# within the window, plus live sessions and jobs updated within the window.
# history.jsonl only records typed prompts, so a session that kept working
# after the last prompt (loops, wakeups, task notifications) would look older
# than it is if the window were applied to it alone; the union covers that.
#
# For each session not marked card=fresh, a slice of the transcript tail is
# written to $CACHE/slices/<sid>.txt for a model to summarise into a state
# card at $CACHE/cards/<sid>.json: title, cwds, branches, the typed prompts in
# the window from history.jsonl, and the last exchanges from the transcript
# tail. Slices hold typed prompts and assistant prose only: tool results are
# where pasted secrets and file contents end up, so they never enter the slice.
# A card is fresh while its transcriptSize matches the transcript on disk.
set -u
DAYS="${1:-7}"
BASE="${2:-$HOME/src/chainguard}"
CLAUDE="$HOME/.claude"
CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/claude-wip"
mkdir -p "$CACHE/cards" "$CACHE/slices"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

now=$(date +%s)
since_s=$(( now - DAYS * 86400 ))
since_ms=$(( since_s * 1000 ))
since_iso=$(date -u -r "$since_s" '+%Y-%m-%dT%H:%M:%SZ')

# Flatten prose to one pipe-safe line, truncated.
flat() { tr '\n\r|' '  /' | sed 's/  */ /g; s/^ //; s/ $//' | cut -c1-"${1:-140}"; }

# Text of a user record's content: typed prompts are strings or text blocks;
# tool results are arrays of tool_result blocks and reduce to "".
USER_TEXT='(.message.content | if type=="string" then . elif type=="array" then (map(select(.type=="text") | .text) | join(" ")) else "" end)'
ASSISTANT_TEXT='(.message.content | if type=="array" then (map(select(.type=="text") | .text) | join(" ")) elif type=="string" then . else "" end)'

# ---- history: typed prompts within the window, aggregated per session ----
jq -r -s --argjson s "$since_ms" '
  map(select(.timestamp >= $s and .sessionId != null))
  | group_by(.sessionId) | .[] | sort_by(.timestamp)
  | [ .[0].sessionId,
      (.[0].timestamp / 1000 | floor | todate | .[0:16]),
      (.[-1].timestamp / 1000 | floor | todate | .[0:16]),
      length,
      .[0].project,
      ((map(.display) | map(select((startswith("/") or startswith("!")) | not)) | first // "") | gsub("[\n\r|\t]"; " ") | .[0:140]) ]
  | @tsv' "$CLAUDE/history.jsonl" 2>/dev/null > "$TMP/hist.tsv"
# Every typed prompt in the window, for the slices: the transcript tail of a
# long session may hold a single exchange, while history has them all.
jq -r --argjson s "$since_ms" '
  select(.timestamp >= $s and .sessionId != null)
  | select((.display | startswith("/") or startswith("!")) | not)
  | [.sessionId, (.timestamp / 1000 | floor | todate | .[0:16]), (.display | gsub("[\n\r\t]+"; " ") | .[0:300])]
  | @tsv' "$CLAUDE/history.jsonl" 2>/dev/null > "$TMP/prompts.tsv"

# ---- live sessions: `claude agents` unioned with the pid registry ----
claude agents --json --all 2>/dev/null > "$TMP/agents.json" || echo '[]' > "$TMP/agents.json"
jq -e 'type == "array"' "$TMP/agents.json" >/dev/null 2>&1 || echo '[]' > "$TMP/agents.json"
for f in "$CLAUDE"/sessions/*.json; do
  [ -f "$f" ] || continue
  pid=$(jq -r '.pid // empty' "$f")
  [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null || continue
  jq -c '{pid, sessionId, kind, status, name, cwd, jobId, parkedJobId}' "$f"
done | jq -s '.' > "$TMP/registry.json"
jq -n --slurpfile a "$TMP/agents.json" --slurpfile r "$TMP/registry.json" '
  def norm: with_entries(select(.value != null and .value != ""));
  ( $a[0] | map(select(.pid != null)
      | {sid: .sessionId, kind: (if .kind == "background" then "bg" else .kind end),
         status: .status, name: .name, cwd: .cwd, jobId: .id, src: "agents"} | norm) )
  + ( $r[0] | map({sid: .sessionId, kind, status, name, cwd, jobId, parked: .parkedJobId, src: "registry"} | norm) )
  | group_by(.sid)
  | map( (reduce .[] as $e ({}; . * ($e | del(.src)))) + {src: (map(.src) | unique | join("+"))} )
' > "$TMP/live.json"

# ---- background jobs ----
for f in "$CLAUDE"/jobs/*/state.json; do
  [ -f "$f" ] || continue
  jq -c '{jobId: .daemonShort, sid: .sessionId, state, updatedAt, cwd,
          name: (.name // ""), intent: (.intent // ""), detail: (.detail // "")}' "$f"
done | jq -s 'map(select(.sid != null))' > "$TMP/jobs.json"

echo "== LIVE =="
jq -r '.[] | [.sid, (.kind // ""), (.status // ""), (.name // ""), (.cwd // ""),
              (.jobId // ""), (.parked // ""), .src] | join("|")' "$TMP/live.json"

echo
echo "== JOBS =="
jq -r --arg s "$since_iso" '.[] | select(.updatedAt >= $s)
  | [.jobId, .sid, .state, (.updatedAt | .[0:16]), (.cwd // ""), .name,
     (.intent | gsub("[\n\r|]"; " ") | .[0:120]),
     (.detail | gsub("[\n\r|]"; " ") | .[0:200])] | join("|")' "$TMP/jobs.json"

# ---- candidate set ----
{
  cut -f1 "$TMP/hist.tsv"
  jq -r '.[].sid' "$TMP/live.json"
  jq -r --arg s "$since_iso" '.[] | select(.updatedAt >= $s) | .sid' "$TMP/jobs.json"
} | grep -E '^[0-9a-f-]{36}$' | sort -u > "$TMP/candidates.txt"

# Worktree paths under BASE (main checkouts included), for the path join.
for repo in "$BASE"/*/; do
  git -C "$repo" rev-parse --git-dir >/dev/null 2>&1 || continue
  git -C "$repo" worktree list --porcelain 2>/dev/null | awk '/^worktree /{print $2}'
done | sort -u > "$TMP/worktrees.txt"

: > "$TMP/sessions.txt"; : > "$TMP/paths.txt"; : > "$TMP/branches.txt"
: > "$TMP/roots.txt"; : > "$TMP/lineage.txt"
while read -r sid; do
  live=$(jq -r --arg s "$sid" '.[] | select(.sid == $s) | (.kind // "") + "-" + (.status // "")' "$TMP/live.json" | head -1)
  job=$(jq -r --arg s "$sid" '.[] | select(.sid == $s) | .state' "$TMP/jobs.json" | head -1)
  case "$live" in
    interactive-*) liveness="live-${live#interactive-}" ;;
    bg-*)          liveness="bg-${live#bg-}" ;;
    *) if [ -n "$job" ]; then liveness="job-$job"; else liveness="ended"; fi ;;
  esac
  hist=$(awk -F'\t' -v s="$sid" '$1 == s' "$TMP/hist.tsv" | head -1)
  first_at=$(printf '%s' "$hist" | cut -f2); last_at=$(printf '%s' "$hist" | cut -f3)
  nprompts=$(printf '%s' "$hist" | cut -f4); [ -z "$nprompts" ] && nprompts=0
  # The first typed prompt comes from history; the transcript head is only a
  # fallback, since attachments and slash commands can push it past the slice.
  first=$(printf '%s' "$hist" | cut -f6)

  t=$(ls "$CLAUDE"/projects/*/"$sid".jsonl 2>/dev/null | head -1)
  # Sessions with no transcript and no process left are noise (aborted starts).
  [ -z "$t" ] && [ "$liveness" = "ended" ] && continue
  title=""; last=""; mtime=""; size=0; card="none"; root=""
  if [ -n "$t" ]; then
    mtime=$(date -u -r "$(stat -f '%m' "$t")" '+%Y-%m-%dT%H:%M')
    size=$(stat -f '%z' "$t")
    # Whole records only: a byte-count cut can land inside a large attachment
    # record and leave jq with unparseable JSON before the first real prompt.
    head -n 80 "$t" > "$TMP/head.jsonl"
    tail -c 400000 "$t" | sed 1d > "$TMP/tail.jsonl"
    root=$(jq -r 'select(.type == "user") | .uuid' "$TMP/head.jsonl" 2>/dev/null | head -1)
    [ -z "$first" ] && first=$(jq -r "select(.type == \"user\" and (.isMeta | not)) | $USER_TEXT as \$t
              | select(\$t != \"\" and (\$t | startswith(\"<\") | not)) | \$t | gsub(\"[\n\r]\"; \" \")" "$TMP/head.jsonl" 2>/dev/null |
            head -1 | flat 140)
    title=$(jq -r 'select(.type == "ai-title") | .aiTitle' "$TMP/tail.jsonl" 2>/dev/null | tail -1 | flat 80)
    last=$(jq -r 'select(.type == "last-prompt") | .lastPrompt // empty' "$TMP/tail.jsonl" 2>/dev/null | tail -1 | flat 140)
    [ -z "$last_at" ] && last_at=$(jq -r 'select(.type == "user") | .timestamp' "$TMP/tail.jsonl" 2>/dev/null | tail -1 | cut -c1-16)
    # One pass over the whole transcript for both sets; everything else reads slices.
    grep -oE '"(cwd|gitBranch)":"[^"]*"' "$t" | sort -u | tr -d '"' > "$TMP/kv.txt"
    awk -F: '$1 == "cwd" { print $2 }' "$TMP/kv.txt" | sed "s#^#$sid|#" >> "$TMP/paths.txt"
    awk -F: '$1 == "gitBranch" && $2 != "HEAD" { print $2 }' "$TMP/kv.txt" | sed "s#^#$sid|#" >> "$TMP/branches.txt"
    [ -n "$root" ] && echo "$root|$size|$sid" >> "$TMP/roots.txt"

    cardf="$CACHE/cards/$sid.json"
    if [ -f "$cardf" ]; then
      if [ "$(jq -r '.transcriptSize // 0' "$cardf" 2>/dev/null)" = "$size" ]; then card="fresh"; else card="stale"; fi
    fi
    if [ "$card" != "fresh" ]; then
      {
        echo "# session $sid  size=$size  liveness=$liveness"
        echo "# title: $title"
        echo "# first prompt: $first"
        echo "# cwds: $(grep "^$sid|" "$TMP/paths.txt" | cut -d'|' -f2 | tr '\n' ' ')"
        echo "# branches: $(grep "^$sid|" "$TMP/branches.txt" | cut -d'|' -f2 | tr '\n' ' ')"
        echo "# typed prompts in the window, oldest first (last 6):"
        awk -F'\t' -v s="$sid" '$1 == s { print "#   " $2 " " $3 }' "$TMP/prompts.tsv" | tail -6
        echo "# --- last exchanges: typed prompts and assistant prose only; tool output excluded ---"
        jq -r "
          if .type == \"user\" and (.isMeta | not) then
            $USER_TEXT as \$t | select(\$t != \"\" and (\$t | startswith(\"<\") | not)) | \"USER: \" + \$t[0:1500]
          elif .type == \"assistant\" then
            $ASSISTANT_TEXT as \$t | select(\$t != \"\") | \"ASSISTANT: \" + \$t[0:1500]
          else empty end" "$TMP/tail.jsonl" 2>/dev/null > "$TMP/exchanges.txt"
        # A long assistant turn is many records, so a plain tail would show no
        # prompts at all. Keep the last prompts and the last replies, in order.
        { grep -n "^USER: " "$TMP/exchanges.txt" | tail -5; grep -n "^ASSISTANT: " "$TMP/exchanges.txt" | tail -8; } |
          sort -t: -k1,1n | cut -d: -f2-
      } > "$CACHE/slices/$sid.txt"
    fi
  fi
  echo "$sid|$liveness|${first_at:--}|${last_at:--}|prompts=$nprompts|${mtime:--}|${size}B|card=$card|$title|first=$first|last=$last" >> "$TMP/sessions.txt"
done < "$TMP/candidates.txt"

echo
echo "== SESSIONS =="
sort -t'|' -k4,4r "$TMP/sessions.txt"

echo
echo "== SESSION PATHS =="
sort -u "$TMP/paths.txt"

echo
echo "== SESSION BRANCHES =="
sort -u "$TMP/branches.txt"

echo
echo "== LINEAGE =="
# Parked: registry entry with parkedJobId -> that job's session.
jq -r 'map(select(.parked != null)) | .[] | [.sid, .parked] | @tsv' "$TMP/live.json" |
  while IFS=$'\t' read -r parent jobid; do
    child=$(jq -r --arg j "$jobid" '.[] | select(.jobId == $j) | .sid' "$TMP/jobs.json" | head -1)
    [ -n "$child" ] && echo "$parent|$child|parked"
  done >> "$TMP/lineage.txt"
# Respawned: a live process serving a job under a different session id than
# the one the job state records.
jq -r 'map(select(.jobId != null)) | .[] | [.sid, .jobId] | @tsv' "$TMP/live.json" |
  while IFS=$'\t' read -r live jobid; do
    orig=$(jq -r --arg j "$jobid" '.[] | select(.jobId == $j) | .sid' "$TMP/jobs.json" | head -1)
    [ -n "$orig" ] && [ "$orig" != "$live" ] && echo "$orig|$live|respawned"
  done >> "$TMP/lineage.txt"
# Shared root: same first user record across transcripts.
sort -t'|' -k1,1 -k2,2n "$TMP/roots.txt" |
  awk -F'|' '{ if ($1 == prev_root) print prev_sid "|" $3 "|shared-root"; prev_root = $1; prev_sid = $3 }' >> "$TMP/lineage.txt"
# One reason per pair: parked and respawned explain the shared root.
sort -u "$TMP/lineage.txt" | awk -F'|' '!($1 "|" $2 in seen) { seen[$1 "|" $2] = 1; print }'

echo
echo "== WORKTREE SESSIONS =="
while IFS='|' read -r sid p; do
  grep -qxF "$p" "$TMP/worktrees.txt" || continue
  line=$(grep "^$sid|" "$TMP/sessions.txt" | head -1)
  echo "$p|$sid|$(printf '%s' "$line" | cut -d'|' -f2)|$(printf '%s' "$line" | cut -d'|' -f4)"
done < "$TMP/paths.txt" | sort -t'|' -k1,1 -k4,4r
exit 0
