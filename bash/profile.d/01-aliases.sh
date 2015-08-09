#!/bin/bash

# enable color support of ls and also add handy aliases
if hash dircolors 2>/dev/null; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

if hash hodor 2>/dev/null; then
    alias t=hodor
fi

if hash apt-get 2>/dev/null; then
    alias sagu='sudo apt-get update'
    alias sagi='sudo apt-get install'
fi
