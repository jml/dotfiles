
if [ -z "${VIRTUALENVWRAPPER_LOCATION}" ]; then
    VIRTUALENVWRAPPER_LOCATION="$(type -p virtualenvwrapper.sh)"
fi

if [ -f "${VIRTUALENVWRAPPER_LOCATION}" ]; then
    export WORKON_HOME=$HOME/.virtualenvs
    export PROJECT_HOME=$HOME/src
    . $VIRTUALENVWRAPPER_LOCATION
else
    echo "Could not find virtualenvwrapper"
fi
