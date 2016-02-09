# Load direnv if installed.
DIRENV_LOCATION="$(type -p direnv)"

if [ -n "$DIRENV_LOCATION" ] && [ -f $(type -p direnv) ]; then
    eval "$(direnv hook bash)"
else
    echo "direnv not installed"
fi

unset DIRENV_LOCATION
