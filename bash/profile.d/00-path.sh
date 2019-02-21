export PATH="$HOME/.cask/bin:$HOME/bin:$HOME/.cabal/bin:$HOME/.local/bin:/usr/local/bin:$PATH"

# Configure GNU coreutils to be on the PATH as the default tools.
HOMEBREW_PREFIX=/usr/local
COREUTILS_DIR=$HOMEBREW_PREFIX/opt/coreutils/libexec

if [ -d ${COREUTILS_DIR} ]; then
    export PATH=${COREUTILS_DIR}/gnubin:${PATH}
    export MANPATH=${COREUTILS_DIR}/gnuman:${MANPATH}
fi
