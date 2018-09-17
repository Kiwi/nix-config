#!/usr/bin/env bash
# ghettolinker.sh - no warranty. :)

# this script to (re)generates symlinks in a user's $HOME based on a dotfiles directory.

# It will run itself in a nixos chroot if it finds itself mounted in /mnt
# (non-nixos users might want to modify or comment the chrootlink() lines)

dotuser="adam"
dotroot="/nix-config/dotfiles"

# array of dirs that should not be created and populated with symlinks, but
# instead just use a directory symlink.
use_dir_symlinks=(".emacs.d/lisp.d"
                  "bin")

# directories with files not to symlink
ignore_dirs=("Private")

# special directions runs last (put any exceptional instructions here.)
special_directions() {
    # symlink everything in my Private/ dir to ~/.
    pvt=(Private/*)
    for p in "${pvt[@]}"
    do
        p=${p##*/}
        ln -sfn ${dotroot}/Private/"$p" /home/${dotuser}/."$p"
    done
}

#####################################################

# initial checks
dotroot=${dotroot%/}

# rerun the script in a nixos-enter (chroot) if my config is found to be in /mnt
chrootlink() {
    cat << EOF | nixos-enter
${dotroot}/bin/ghettolinker.sh
exit
EOF
}
[[ -d /mnt/${dotroot} ]] && chrootlink

# go to right place.
cd "$dotroot" || exit

# make empty dirtree
dirray=($(find . -type d | grep "./"))

# remove ignore dirs from tree
for ignore_dir in "${ignore_dirs[@]}"; do
    dirray=(${dirray[@]//*$ignore_dir*})
done

for d in "${dirray[@]}"
do
    d=$(echo "$d" | sed 's|^./||')
    d=${d%/}
    mkdir -p /home/"${dotuser}"/"$d"
done

# populate dirtree with symlinks
filray=($(find . -type f | grep "./"))
for f in "${filray[@]}"
do
    f=$(echo "$f" | sed 's|^./||')
    f=${f%/}
    ln -sf ${dotroot}/"$f" /home/${dotuser}/"$f"
done

# use directory symlinks for exceptions specified in the array.
for spdir in "${use_dir_symlinks[@]}"
do
    spdir=$(echo "$spdir" | sed 's|^./||')
    spdir=${spdir%/}
    # del the real directory full of symlinks
    rm -rf /home/${dotuser:?}/"${spdir}"
    # just symlink the directory full of real files instead
    ln -sfn ${dotroot}/"$spdir" /home/${dotuser}/"$spdir"
done

special_directions
