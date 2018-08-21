if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# Put your fun stuff here.

#functions
watch() { while true; do "$@"; sleep 2; done; }
beadm() { /usr/local/sbin/beadm "$@" && /usr/local/sbin/grubbe-mkconfig > /boot/grub/grub.cfg; }
termbin() {
    sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g" | \
        tee ~/.teefile && nc termbin.com 9999 < ~/.teefile
}
gg() { git grep $@ $(git rev-list --all); }

#aliases
alias dot='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias dcap='dot commit -a && dot push'
alias socks_open='ssh -D 1337 -C -N'

#output error codes
EC() { echo -e '\e[1;33m'$?'\e[m'; }
trap EC ERR
