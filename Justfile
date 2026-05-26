# Install everything

target := env_var_or_default('STOW_TARGET', env_var('HOME'))

# macOS workstation setup (includes aerospace window manager)
all: emacs claude
    stow -t {{target}} -d configs aerospace git zsh ghc

# Linux/Pi setup (no macOS-specific configs)
linux: emacs claude
    stow -t {{target}} -d configs git zsh

emacs:
    stow -t {{target}} -d configs --no-folding emacs

# emacs-plus ships both bundles in its keg but does not install them; copying lets Launch Services
# offer them as handlers. Emacs Client.app is a thin droplet around emacsclient, so it survives upgrades.
# Copy the Emacs apps into /Applications and make Emacs Client the .md handler (macOS)
emacs-apps:
    #!/usr/bin/env bash
    set -euo pipefail
    prefix="$(brew --prefix emacs-plus)"
    ditto "$prefix/Emacs.app" "/Applications/Emacs.app"
    ditto "$prefix/Emacs Client.app" "/Applications/Emacs Client.app"
    duti -s org.gnu.EmacsClient .md all

claude:
    stow -t {{target}} -d configs --no-folding claude

# Remove all symlinks
clean:
    stow -t {{target}} -d configs -D aerospace claude emacs git zsh ghc
