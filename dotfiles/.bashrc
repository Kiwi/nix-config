if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# Put your fun stuff here.

watch() { while true; do "$@"; sleep 2; done; }
gg() { git grep $@ $(git rev-list --all); }

alias socks_open='ssh -D 1337 -C -N'

#output error codes
EC() { echo -e '\e[1;33m'$?'\e[m'; }
trap EC ERR
