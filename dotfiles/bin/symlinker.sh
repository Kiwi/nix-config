#!/usr/bin/env bash

# dotroot
dotuser="adam"
dotroot="/nix-config/dotfiles"

# array of dirs that should not be created and populated with symlinks, but
# instead just use a directory symlink.
use_dir_symlinks=(".emacs.d/lisp.d"
                  "bin")

ignore_dirs=("Private")

#####################################################

#initial checks
[[ $THEMELIOS_INSTALL ]] && mnt="/mnt"
dotroot=${dotroot%/}

# go to right place.
cd "${mnt}$dotroot" || exit

# make empty dirtree
dirray=($(find . -type d | grep "./"))
for d in "${dirray[@]}"
do
    d=$(echo "$d" | sed 's|^./||')
    d=${d%/}
    mkdir -p "${mnt}"/home/"${dotuser}"/test/"$d"
done

# populate dirtree with symlinks
filray=($(find . -type f | grep "./"))
for f in "${filray[@]}"
do
    f=$(echo "$f" | sed 's|^./||')
    f=${f%/}
    ln -sf ${mnt}${dotroot}/"$f" ${mnt}/home/${dotuser}/test/"$f"
done

# use directory symlinks for exceptions specified in the array.
for spdir in "${use_dir_symlinks[@]}"
do
    spdir=$(echo "$spdir" | sed 's|^./||')
    spdir=${spdir%/}
    # del the real directory full of symlinks
    rm -rf ${mnt}/home/${dotuser}/test/"${spdir}"
    # just symlink the directory full of real files instead
    ln -sfn ${mnt}${dotroot}/"$spdir" ${mnt}/home/${dotuser}/test/"$spdir"
done
