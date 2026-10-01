#!/bin/sh
# Claude Code status line — spaceship-inspired
# Receives JSON on stdin; outputs a single status line.

input=$(cat)

# Current directory (prefer project_dir for the git root feel, fall back to cwd)
cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd')
# Show home-dir-relative path like spaceship does
home="$HOME"
display_dir="${cwd/#$home/~}"

# Git repo and branch info (from workspace)
repo=$(echo "$input" | jq -r '.workspace.repo | if . then .owner + "/" + .name else "" end')
worktree=$(echo "$input" | jq -r '.workspace.git_worktree // ""')

# Model display name
model=$(echo "$input" | jq -r '.model.display_name // ""')

# Context remaining (only shown once there are messages)
ctx=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')

# Session cost, estimated client-side (may differ from the actual bill)
cost=$(echo "$input" | jq -r '.cost.total_cost_usd // empty')

# Wall-clock session duration, in whole seconds
dur_secs=$(echo "$input" | jq -r '(.cost.total_duration_ms // empty) / 1000 | floor')

# Lines added/removed across the session
added=$(echo "$input" | jq -r '.cost.total_lines_added // 0')
removed=$(echo "$input" | jq -r '.cost.total_lines_removed // 0')

# Build the line
line=""

# Directory segment
line=$(printf '\033[34m%s\033[0m' "$display_dir")

# Repo/branch segment
if [ -n "$repo" ]; then
  branch_part="$repo"
  if [ -n "$worktree" ]; then
    branch_part="$branch_part ($worktree)"
  fi
  line="$line $(printf '\033[33m\xee\x82\xa0 %s\033[0m' "$branch_part")"
fi

# Model segment
if [ -n "$model" ]; then
  line="$line $(printf '\033[35m%s\033[0m' "$model")"
fi

# Context remaining
if [ -n "$ctx" ]; then
  ctx_int=$(printf '%.0f' "$ctx")
  if [ "$ctx_int" -lt 20 ]; then
    color='\033[31m'  # red when low
  else
    color='\033[32m'  # green otherwise
  fi
  line="$line $(printf "${color}ctx:%s%%\033[0m" "$ctx_int")"
fi

# Session cost
if [ -n "$cost" ]; then
  line="$line $(printf '\033[36m$%.2f\033[0m' "$cost")"
fi

# Session duration
if [ -n "$dur_secs" ]; then
  if [ "$dur_secs" -ge 3600 ]; then
    dur=$(printf '%dh%dm' $((dur_secs / 3600)) $((dur_secs % 3600 / 60)))
  elif [ "$dur_secs" -ge 60 ]; then
    dur="$((dur_secs / 60))m"
  else
    dur="${dur_secs}s"
  fi
  line="$line $(printf '\033[90m%s\033[0m' "$dur")"
fi

# Lines changed (hidden until the session has edited something)
if [ "$added" -gt 0 ] || [ "$removed" -gt 0 ]; then
  line="$line $(printf '\033[32m+%s\033[0m/\033[31m-%s\033[0m' "$added" "$removed")"
fi

printf '%s' "$line"
