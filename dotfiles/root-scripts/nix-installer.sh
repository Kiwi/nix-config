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
# It is also recommended to give ZFS entire disks, they will be GPT partitioned automatically by ZFS.
POOL_DISKS="
/dev/sda
/dev/sdb
"

# "legacy" or "uefi" !! legacy recommended if you can help it.
BIOS_TYPE="legacy"

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
    sed -i '/imports/a \
boot.supportedFilesystems = [ \"zfs\" ];' /etc/nixos/configuration.nix
    nixos-rebuild switch
}

__poolcreate() {
    echo "Should atime be \"on\" or \"off\"? (atime=off is better for SSD life.)"
    echo "Type \"on\" or \"off\" and press [ENTER]:"

    # sanity check response
    read -r ATIME
    if [[ ${ATIME} != "on" && ${ATIME} != "off" ]]
    then
        echo "invalid response, try again."
        __poolcreate
    else
        echo "creating ZPOOL..."
        zpool create -f \
              -o ashift=12 \
              -O compression=lz4 \
              -O atime=${ATIME} \
              -O relatime=on \
              -O normalization=formD \
              -O xattr=sa \
              -m none \
              -R /mnt \
              ${POOL_NAME:?"Please define pool name."} \
              ${POOL_TYPE} \
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

 # begin pool create interactive ()
 __poolcreate

 # now that we have zfs installed to the live disk, we need to setup a zpool on
 # our disk(s) where we will subsequently boot strap our own configuration.nix
 # via github.
 #git clone ${NIXCFG_REPO} /mnt/${NIXCFG_LOCATION}

 #ready2
