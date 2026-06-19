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

printf '%s' "$line"
