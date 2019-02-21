
if [ -n "$PS1" ]; then
    _num_colors=$(tput colors)
    if [[ ${_num_colors} -ge 8 ]]; then
        if type __git_ps1 > /dev/null; then
            PS1='\[\e[1;32m\]\u@\h\[\e[0m\]:\[\e[1;34m\]\w$(__git_ps1)\[\e[0m\]\n\$ '
        else
            PS1='\[\e[1;32m\]\u@\h\[\e[0m\]:\[\e[1;34m\]\w\[\e[0m\]\n\$ '
        fi
    else
        if type __git_ps1 > /dev/null; then
            PS1='\u@\h:\w$(__git_ps1)\n\$ '
        else
            PS1='\u@\h:\w\n\$ '
        fi
    fi
fi

# Simplifies the prompt if the terminal is a bit stupid. Mostly useful for
# enabling tramp.
case "$TERM" in
  "dumb")
    PS1="> "
    ;;
  xterm*|rxvt*|eterm*|screen*)
    ;;
  *)
    PS1="> "
    ;;
esac
