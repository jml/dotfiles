
export PATH="$HOME/.cask/bin:$HOME/bin:$HOME/Library/Haskell/bin:$HOME/.cabal/bin:/usr/local/opt/coreutils/libexec/gnubin:/usr/local/bin:$PATH"

if [ -e /Users/jml/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/jml/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export NIX_PATH=/Users/jml/nixpkgs:nixpkgs=/Users/jml/nixpkgs

export EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"

alias ls="ls --color=auto"
alias t=hodor


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
  _lhs=$(dircolors --print-database)
  _term=${TERM//[^[:alnum:]]/?}
  if [[ $'\n'${_lhs}$'\n' == *$'\n'"TERM "${_term}$'\n'* ]]; then
    PS1='\[\e[1;32m\]\u@\h\[\e[0m\]:\[\e[1;34m\]\w$(__git_ps1)\[\e[0m\]\n\$ '
  else
    PS1='\u@\h:\w$(__git_ps1)\n\$ '
  fi
fi

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

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"


if [ -f ~/.git-completion.bash ]; then
  . ~/.git-completion.bash
fi
