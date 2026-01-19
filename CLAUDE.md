# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal dotfiles repository for managing macOS configuration files using GNU Stow.
The repository contains configuration files for various development tools and applications, organized in a modular structure under the `configs/` directory.

## Repository Structure

- `configs/` - Contains all configuration files organized by application
  - `aerospace/` - AeroSpace window manager configuration
  - `claude/` - Claude Code CLI configuration
  - `emacs/` - Emacs configuration with modular config.d structure
  - `git/` - Git configuration files
  - `ghc/` - GHC Haskell compiler configuration
  - `zsh/` - Zsh shell configuration with Oh My Zsh
- `macos/` - macOS-specific files
  - `Brewfile` - Homebrew package definitions
  - `Brewfile.personal` - Personal Homebrew packages
- `Justfile` - Task runner for installing configurations
- `README.md` - Detailed setup instructions for new macOS systems

## Key Commands

### Installing Configurations

```bash
# Install all configurations (from repository root)
just all

# Install just Emacs (uses --no-folding for Emacs specifically)
just emacs

# Install just Claude Code config
just claude

# Remove all symlinks
just clean
```

### Package Management
```bash
# Install Homebrew packages (from macos/ directory)
cd macos && brew bundle
```

## Architecture and Configuration System

### Stow-based Management
The repository uses GNU Stow to create symlinks from the `configs/` directory to the home directory.
Each subdirectory in `configs/` represents a "package" that can be independently installed or removed.

### Emacs Configuration Structure
The Emacs configuration uses a modular approach:
- `init.el` - Bootstrap file that loads configuration modules
- `config.d/` - Numbered configuration modules (e.g., `01-appearance.el`, `20-lsp.el`)
- `plugins/` - Custom Emacs Lisp utilities
- `custom.el` - Emacs customization file

### AeroSpace Window Manager
AeroSpace is configured as an i3-equivalent for macOS with:
- Dvorak-optimized keybindings (hjkl → jkl;)
- Service and resize modes for advanced window management

### Shell Environment
Zsh configuration includes:
- Oh My Zsh with Spaceship theme
- Plugins: git, direnv, emacs, colorize, pyenv, gcloud, nvm, aws, iterm2
- iTerm2 shell integration enabled

### Claude Code Configuration
The `configs/claude/.claude/` directory contains:
- `CLAUDE.md` - Global instructions that apply to all projects (tone, coding style, workflow rules)
- `settings.json` - User preferences: model choice, enabled plugins, and command permissions

**Permission philosophy:** The settings pre-approve read-only operations for common tools (gcloud, gh, gsutil, bq, terraform) while explicitly denying dangerous operations like `terraform apply -auto-approve`.

**Note:** Per-project instructions go in each project's own `CLAUDE.md` file, not in this dotfiles config.

## Environment Setup Notes

This repository is designed for macOS development environments and includes:
- Development tools (git, ripgrep, jq, awscli, etc.)
- Language runtimes (Go, Python via pyenv, Node via nvm)
- GUI applications (Emacs, iTerm2, 1Password, OmniFocus)
- Window management (AeroSpace)

The setup process involves manual steps for macOS system configuration, Homebrew installation, SSH key generation, and application-specific configurations as detailed in the README.md.
