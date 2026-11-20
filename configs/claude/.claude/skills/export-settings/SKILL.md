---
name: export-settings
description: Sorts the changes in the live ~/.claude/settings.json that the dotfiles layers don't hold into the public dotfiles base, the private overlay, or local-only, then writes the ones jml approves. Use when jml says "/export-settings", "export my settings", "save my Claude settings to dotfiles", or "my settings have drifted", or when `just claude-settings-check` reports changes.
---

# Export settings

`~/.claude/settings.json` is a real file that `claude-settings/apply` in the dotfiles repo builds from three layers, later layers winning:

1. **live**: `~/.claude/settings.json` itself. Claude Code edits it through `/model`, `/config`, `/permissions`, "always allow" answers, and `/auto-mode-setup`.
2. **base**: `claude-settings/base.json` in the dotfiles repo. **The repo, jml/dotfiles, is public on GitHub.**
3. **overlay**: `~/.claude/settings.private.json`. It's untracked and exists on one machine. It holds settings that must not be public, such as the `autoMode` block, and local values for keys the base sets.

Lists directly under `permissions` combine across layers. Every other value takes the later layer's value. So when the live file changes a key the base or overlay sets, the next apply reverts it. This skill moves those changes into a layer before that happens.

## 1. Find the drift

```bash
repo="$(git -C "$(dirname "$(realpath ~/.claude/CLAUDE.md)")" rev-parse --show-toplevel)"
"$repo/claude-settings/apply" --drift
```

The output is a JSON array with one entry per difference:

- `local-only`: the live file sets `path` and neither layer does.
- `changed`: both set `path`, and the live value is `live` while the layers give `layered`.
- `added`: `live` is an entry in the `permissions` list at `path` that no layer has.

An empty array means there's nothing to export. Say so and stop.

## 2. Sort each entry

Give each entry one destination:

- **base**: a preference that makes sense on any of jml's machines and names nothing internal. Examples: `model`, `outputStyle`, `theme`, `effortLevel`, a broad read-only permission such as `Bash(gh pr view:*)`, and a hook that runs a script stowed from the dotfiles repo.
- **overlay**: anything that names employer-internal things, such as:
  - private repositories, people, or customers
  - hostnames, cloud account or project IDs, or bucket names
  - internal registries or MCP servers
  - anything under `autoMode`, whose `environment` describes work infrastructure

  A local value for a key the base sets also goes here, because the overlay is the only per-machine layer that wins over the base.
- **local**: a one-off that only means something on this machine, such as a permission for one command on one absolute path (`Bash(find /Users/jml/src/chainguard/mono/env -name "*.tf" ...)`). It stays in the live file. A `changed` entry can't be local: the next apply reverts it. Sort it as overlay, or drop it.
- **drop**: a `changed` entry jml doesn't want to keep. The next apply restores the layered value.

When an entry could be base or overlay, choose overlay. When it could be base or local, ask. When a value names a repository, check its visibility with `gh repo view <owner>/<name> --json visibility` before sorting it as base.

Show jml a table with these columns:
- the path
- the value, shortened to about 60 characters
- the kind
- the proposed destination
- a short reason

Wait for jml to approve or move entries. Write nothing before then.

## 3. Write the approved entries

Write each file through a temporary file in the same directory, so a failed `jq` doesn't truncate it. For each entry, set `"$v"` to its `live` value as JSON and `$path` to its path:

```bash
# base: set a value at a path
jq --argjson v "$v" --argjson p "$path" 'setpath($p; $v)' "$repo/claude-settings/base.json" > "$tmp" && mv "$tmp" "$repo/claude-settings/base.json"
# base: add a permissions entry
jq --argjson v "$v" --argjson p "$path" 'setpath($p; (getpath($p) // []) + [$v])' "$repo/claude-settings/base.json" > "$tmp" && mv "$tmp" "$repo/claude-settings/base.json"
```

Use the same commands on `~/.claude/settings.private.json` for overlay entries, creating the file as `{}` with mode 600 when it doesn't exist. Local entries need no write. Dropped entries need no write either: the next apply restores the layered value.

## 4. Check the base for leaks

Read every added line of `git -C "$repo" diff claude-settings/base.json`. Look for:
- 12-digit numbers, which are often cloud account IDs
- `*.run.app` and other internal hostnames
- bucket names
- private repository names
- email addresses
- tokens

If anything matches, move that entry to the overlay and tell jml.

## 5. Apply and commit

Run `"$repo/claude-settings/apply"`. It should report that the file is up to date, or show only the entries you just dropped being restored. Then run `"$repo/claude-settings/apply" --drift` again. It should list only the entries sorted as local.

If the base changed, stage only `claude-settings/base.json` and commit it with a conventional commit message, such as `chore(claude): export outputStyle and gh read permissions`. Don't push. Tell jml what went where.
