if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# Put your fun stuff here.

eval `keychain --eval id_rsa`

watch() { while true; do "$@"; sleep 2; done; }
gg() { git grep "$@" "$(git rev-list --all)"; }
ch_bind() { mount -t proc none proc ; mount --rbind /sys sys ; mount --rbind /dev dev; }
ch_ubind() { umount -lR {dev,proc,sys}; }
ch_root() { env -i HOME=/root TERM="$TERM" "$(which chroot)" . bash -l; }
alias keyswaps="xset r rate 250 50; setxkbmap -option ctrl:swap_lalt_lctl -option caps:swapescape"

extract () {
    if [ -f $1 ] ; then
	case $1 in
	    *.tar.bz2)	        tar xjf $1					;;
	    *.tar.gz)	        tar xzf $1					;;
	    *.bz2)		bunzip2 $1					;;
	    *.rar)		rar x $1					;;
	    *.gz)		gunzip $1					;;
	    *.tar)		tar xf $1					;;
	    *.tbz2)		tar xjf $1					;;
	    *.tgz)		tar xzf $1					;;
	    *.zip)		unzip $1					;;
	    *.Z)		uncompress $1					;;
	    *)		echo "'$1' cannot be extracted via extract()" 	;;
	esac
    else
	echo "'$1' is not a valid file"
    fi
}

#output error codes
EC() { echo -e '\e[1;33m'$?'\e[m'; }
trap EC ERR
