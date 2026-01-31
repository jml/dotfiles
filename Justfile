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

claude:
    stow -t {{target}} -d configs --no-folding claude

# Remove all symlinks
clean:
    stow -t {{target}} -d configs -D aerospace claude emacs git zsh ghc
