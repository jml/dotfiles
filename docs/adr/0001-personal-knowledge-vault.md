# ADR 0001: Personal knowledge vault with Emacs and plain Markdown

## Status

Accepted (2026-03-18)

## Context

I want to write more and have been using a PKM (personal knowledge management)
system to support that. Previous attempts have stalled — partly from
overthinking the system (Evergreen Notes, Zettelkasten), partly from
perfectionism, and partly from tooling churn between Org-roam and Obsidian.

I had separate personal and work vaults, which created constant sorting overhead
at capture time. The personal vault contained genuinely private material
(journal, health, finances) alongside general thinking that happened to not be
work-specific.

I also had an unused `obsidian.el` integration in Emacs and a full `org-roam`
setup that I'd moved away from.

## Decision

**One vault of plain Markdown files**, edited in Emacs with the existing
Tufte-themed `markdown-mode` setup. No Obsidian app, no org-roam, no
special PKM tooling beyond wiki links and file search.

Key choices:

- **Plain Markdown over Org** — easier for AI tools to work with, and prefer the
  standard to the offbeat.
- **Wiki links enabled** in markdown-mode for lightweight linking between notes.
- **Git-synced** between machines via a private repo. A `private/` directory
  (journal, health, finances) is `.gitignore`d so it never reaches work
  hardware. This addresses the MDM / device reclamation risk without maintaining
  two separate vaults.
- **No elaborate capture/refile workflow** up front. Start writing, add structure
  when the lack of it actually hurts.
- **Removed `obsidian.el` and `elgrep`** from Emacs config — not needed for a
  plain vault.

## Consequences

- All thinking goes in one place. No routing decision at capture time.
- `private/` needs its own backup mechanism (iCloud, separate encrypted backup)
  since git won't carry it.
- Graph/backlinks features from Obsidian and org-roam are gone. `consult-ripgrep`
  on the vault is the substitute. If discovery becomes a real problem with
  hundreds of notes, revisit then.
- The barrier to writing is now just time and habit, not tooling. That's the
  point.
