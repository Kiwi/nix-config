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

# leave empty for a single disk, mirror for two disks, raidz1 for 3 disks, etc...
POOL_TYPE=""

# It is recommended to list disks using /dev/disk/by-id (instead of e.g. /dev/sda /dev/sdb ...
POOL_DISKS="
/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76820C5544
/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76822C9FD0
"

# Define a variable with the pool device layout in the form of ONE of the following:
#ZVDEVS_zroot="mirror /dev/disk/by-id/... /dev/disk/by-id/..."
#ZVDEVS_zroot="mirror /dev/disk/by-id/... /dev/disk/by-id/... mirror /dev/disk/by-id/... /dev/disk/by-id/..."
#ZVDEVS_zroot="raidz1 /dev/disk/by-id/... /dev/disk/by-id/... /dev/disk/by-id/..."
#ZVDEVS_zroot="raidz2 /dev/disk/by-id/... /dev/disk/by-id/... /dev/disk/by-id/... /dev/disk/by-id/..."
#ZVDEVS_zroot="raidz3 /dev/disk/by-id/... /dev/disk/by-id/... /dev/disk/by-id/... /dev/disk/by-id/... /dev/disk/by-id/..."
#ZVDEVS_zroot="raidz1 /dev/disk/by-id/... /dev/disk/by-id/... /dev/disk/by-id/... raidz1 /dev/disk/by-id/... /dev/disk/by-id/... /dev/disk/by-id/..."

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
    zpool create -f -o \
          ashift=12 -o \
          cachefile=/tmp/zpool.cache \
          -O compression=lz4 \
          -O atime=on \
          -O relatime=on \
          -O normalization=formD \
          -O xattr=sa \
          -m none \
          -R /mnt \
          ${POOL_NAME:?"Please define pool name."} \
          ${POOL_TYPE:?"Please define pool type."} \
          ${POOL_DISKS:?"Please define pool disks."}
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

    echo "Would you like to write zeros to all disks? (Warning: slow operation.)"
    read -p "Continue to writing zeros? (Y or N) " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        IFS=$'\n'
        for DISK_ID in ${POOL_DISKS}
        do
            dd if=/dev/zero of=${DISK_ID} bs=4096 status=progress
        done
    fi
}


# intall zfs to the livedisk, but only if it needs it.
which zfs > /dev/null 2>&1 || __bootstrapzfs

# begin disk prep ()
__diskprep



# now that we have zfs installed to the live disk, we need to setup a zpool on
# our disk(s) where we will subsequently boot strap our own configuration.nix
# via github.
#git clone ${NIXCFG_REPO} /mnt/${NIXCFG_LOCATION}
