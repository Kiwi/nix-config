if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# Put your fun stuff here.

watch() { while true; do "$@"; sleep 2; done; }
gg() { git grep "$@" "$(git rev-list --all)"; }
ch_bind() { mount -t proc none proc ; mount --rbind /sys sys ; mount --rbind /dev dev; }
ch_ubind() { umount -lR {dev,proc,sys}; }
ch_root() { env -i HOME=/root TERM="$TERM" "$(which chroot)" . bash -l; }
alias socks_open='ssh -D 1337 -C -N'

#output error codes
EC() { echo -e '\e[1;33m'$?'\e[m'; }
trap EC ERR
