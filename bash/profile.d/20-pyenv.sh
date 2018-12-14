# Set up pyenv - https://github.com/pyenv/pyenv
#
# Manages versions of Python installed on the system.
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi
