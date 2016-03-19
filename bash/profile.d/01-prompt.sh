
# From http://rusty.ozlabs.org/?p=359
# Git me harder!
__git_ps1 ()
{
    local g="$(git rev-parse --git-dir 2>/dev/null)"
    if [ -n "$g" ]; then
        local r
        local b
        if [ -d "$g/../.dotest" ]
        then
            local b="$(git symbolic-ref HEAD 2>/dev/null)"
            r="|REBASING"
        elif [ -d "$g/.dotest-merge" ]
        then
            r="|REBASING"
            b="$(cat $g/.dotest-merge/head-name)"
        elif [ -f "$g/MERGE_HEAD" ]
        then
            r="|MERGING"
            b="$(git symbolic-ref HEAD 2>/dev/null)"
        else
            if [ -f $g/BISECT_LOG ]
            then
                r="|BISECTING"
            fi
            if ! b="$(git symbolic-ref HEAD 2>/dev/null)"
            then
                b="$(cut -c1-7 $g/HEAD)..."
            fi
        fi
        if [ -n "$1" ]; then
            printf "$1" "${b##refs/heads/}$r"
        else
            printf " (%s)" "${b##refs/heads/}$r"
        fi
    fi
}


if [ -n "$PS1" ]; then
    _num_colors=$(tput colors)
    if [[ ${_num_colors} -ge 8 ]]; then
        PS1='\[\e[1;32m\]\u@\h\[\e[0m\]:\[\e[1;34m\]\w$(__git_ps1)\[\e[0m\] ($?)\n\$ '
    else
        PS1='\u@\h:\w$(__git_ps1) ($?)\n\$ '
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
