#!/usr/bin/env bash

# Copyright 2018 sch@efers.org
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# A nixos zfs-on-root shell script installer.

# instructions:
# Set variables below and then execute the script from a nixos live disk,
# e.g. curl https://github.com/a-schaefers/nix-config/nix-installer.sh | sh
# An internet connection is required.

#################
# set variables #
#################

# Your personal nix-config repo
NIXCFG_REPO="git@github.com:a-schaefers/nix-config.git"

# Preferred location where we will clone the nix-config repo
NIXCFG_LOCATION="/nix-config/"

# Pool name preference, e.g. rpool or zroot
POOL_NAME="zroot"

# leave empty "" for single disk or use "mirror", "raidz1", "raidz2", or "raidz3"
POOL_TYPE="mirror"

# It is recommended to list disks using /dev/disk/by-id (instead of e.g. /dev/sda /dev/sdb ...
POOL_DISKS="
/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76820C5544
/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76822C9FD0
"

##############################################################################
# script                                                                     #
##############################################################################

echo "WARNING: The following script is an operating system installer, previous data will be unrecoverable."
read -p "Continue? (Y or N) " -n 1 -r
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    echo "aborted" ; exit
fi

__bootstrapzfs() {
    echo '{ ... }:
{ imports = [
      /etc/nixos/hardware-configuration.nix
          ];
boot.supportedFilesystems = [ "zfs" ];
}' > /etc/nixos/configuration.nix
    nixos-rebuild switch
}

__poolcreate() {
    echo "Should atime be \"on\" or \"off\"? (atime=off is better for SSD life.)"
    echo "Type \"on\" or \"off\" and press [ENTER]:"

    # sanity check response
    read ATIME
    if [[ ${ATIME} != "on" && ${ATIME} != "off" ]]
    then
        __poolcreate
    else
        # create the pool
        zpool create -f -o \
              ashift=12 -o \
              -O compression=lz4 \
              -O atime=${ATIME} \
              -O relatime=on \
              -O normalization=formD \
              -O xattr=sa \
              -m none \
              -R /mnt \
              ${POOL_NAME:?"Please define pool name."} \
              ${POOL_TYPE:?"Please define pool type."} \
              ${POOL_DISKS:?"Please define pool disks."}
    fi
}

__diskprep() {
    echo "If giving entire disks to ZFS, it is a good idea first to remove various remnants from previous operating system installations."
    echo "If using new disks for the first time, this is not necessary."
    read -p "Continue to remove disk signatures and remnants? (Y or N) " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        IFS=$'\n'
        for DISK_ID in ${POOL_DISKS}
        do
            sgdisk --clear ${DISK_ID}
            wipefs --all ${DISK_ID}
        done
    fi
}


# intall zfs to the livedisk, but only if it needs it.
which zfs > /dev/null 2>&1 || __bootstrapzfs

# begin disk prep interactive ()
__diskprep



# now that we have zfs installed to the live disk, we need to setup a zpool on
# our disk(s) where we will subsequently boot strap our own configuration.nix
# via github.
#git clone ${NIXCFG_REPO} /mnt/${NIXCFG_LOCATION}
