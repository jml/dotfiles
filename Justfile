# Install everything

target := env_var_or_default('STOW_TARGET', env_var('HOME'))

all: emacs
    stow -t {{target}} -d configs aerospace git zsh ghc

emacs:
    stow -t {{target}} -d configs --no-folding emacs

# Remove all symlinks
clean:
    stow -t {{target}} -d configs -D aerospace emacs git zsh ghc
