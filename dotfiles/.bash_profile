[[ -f ~/.bashrc ]] && . ~/.bashrc

[[ ! $DISPLAY && $XDG_VTNR -eq 1 ]] && sway
