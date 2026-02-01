# Homebrew (must come first to set up PATH for other tools)
if [[ -f /opt/homebrew/bin/brew ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
elif [[ -f /usr/local/bin/brew ]]; then
    eval "$(/usr/local/bin/brew shellenv)"
fi

# Set up pyenv for login shells (before .zshrc runs)
# This ensures pyenv is in PATH when oh-my-zsh loads
if [[ -d "$HOME/.pyenv" ]]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init --path)"
fi

# Machine-local config (for env vars like NOTEBOOK_POSTS_DIR)
[[ -f ~/.zprofile.local ]] && source ~/.zprofile.local
