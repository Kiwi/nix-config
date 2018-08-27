#!/usr/bin/env bash

# Copyright 2018 sch@efers.org
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# A simple nix-on-zroot installer.

# instructions:
# Set variables below and then execute the script from a nixos live disk.

#################
# set variables #
#################

# "legacy" or "uefi" !! legacy recommended if you can help it.
BIOS_TYPE="legacy"

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

# Your personal nix-config repo
NIXCFG_REPO="git@github.com:a-schaefers/nix-config.git"

# Preferred location where we will clone the nix-config repo
NIXCFG_LOCATION="/nix-config/"

##############################################################################
# script                                                                     #
##############################################################################

echo "WARNING: The following script intends to replace all of your disk(s) contents with a fresh zfs-on-root NixOS installation."
echo ""
read -p "Continue? (Y or N) " -n 1 -r
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    echo "Aborted." ; exit
fi

__bootstrapzfs() {
    sed -i '/imports/a \
boot.supportedFilesystems = [ \"zfs\" ];' /etc/nixos/configuration.nix
    nixos-rebuild switch
}

__disk_prep() {
    echo ""
    echo "If giving entire disks to ZFS, it is a good idea first to remove various \
 remnants from previous operating system installations."
    echo "If using new disks for the first time, this is not necessary."
    echo ""
    echo "WARNING: This step will make any existing data on the specified drive \
inaccesable, but will not securely delete it! If you would like to securely erase \
 data, use a secure erase tool, such as nwipe."
    echo ""

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

__zpool_create() {
    echo ""
    echo "Should atime be \"on\" or \"off\"? (atime=off is better for SSD life.)"
    echo "Type \"on\" or \"off\" and press [ENTER]:"

    read -r ATIME
    if [[ ${ATIME} != "on" && ${ATIME} != "off" ]]
    then
        echo "invalid response."
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

__datasets_create() {
    # ROOT
    zfs create -o mountpoint=none -o canmount=off ${POOL_NAME}/ROOT
    zfs create -o mountpoint=legacy -o canmount=on ${POOL_NAME}/ROOT/nixos
    mount -t zfs ${POOL_NAME}/ROOT/nixos /mnt

    # HOME
    mkdir /mnt/home
    mkdir /mnt/root
    __primary_user() {
        echo ""
        echo "Type your primary non-root user name (will be used for the home dataset name) and press [ENTER]:"
        read -r PRIMARY_USER
        echo is ${PRIMAR_USER} okay?
        read -p "(Y or N) " -n 1 -r
        if [[ ! $REPLY =~ ^[Yy]$ ]]
        then
            __primary_user
        fi
    }
    __primary_user

    zfs create -o mountpoint=none -o canmount=off ${POOL_NAME}/HOME
    zfs create -o mountpoint=legacy -o canmount=on ${POOL_NAME}/HOME/root
    zfs create -o mountpoint=legacy -o canmount=on ${POOL_NAME}/HOME/${PRIMARY_USER}
    mount -t zfs ${POOL_NAME}/HOME/root /mnt/root
    mount -t zfs ${POOL_NAME}/HOME/${PRIMARY_USER} /mnt/${PRIMARY_USER}

    # /NIX option
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "The following option is a tradeoff that is NOT RECOMMENDED."
    echo "Answering yes, trades some zfs robustness for extra disk space savings."
    echo ""
    echo "Answering yes is only useful if you do not garbage collect your NixOS install regularly or if you"
    echo "intend to keep long-term nix-store snapshots and have storage capacity concerns."
    echo ""
    read -p "Mount /nix store outside of ROOT dataset? Not recommended. (Y or N) " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        zfs create -o mountpoint=legacy -o canmount=on ${POOL_NAME}/NIX
        mkdir /mnt/nix
        mount -t zfs ${POOL_NAME}/nix /mnt/nix
    fi

    # /BOOT/grub option
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "While this may be recommended for good reasons on some non-NixOS systems, with NixOS and GRUB it is currently a bad option."
    echo ""
    read -p "Mount /boot/grub/ outside of ROOT dataset? Not recommended. (Y or N) " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        zfs create -o mountpoint=none -o canmount=off ${POOL_NAME}/BOOT
        zfs create -o mountpoint=legacy -o canmount=on ${POOL_NAME}/BOOT/grub
        mkdir -p /mnt/boot/grub
        mount -t zfs ${POOL_NAME}/BOOT/grub /mnt/boot/grub
    fi

    # zfs-auto-snapshot

    echo ""
    echo "com.sun:auto-snapshot is used by the nixos built-in services.zfs.autoSnapshot"
    echo "so if you want your nixos zfs.autoSnapshot settings to apply to ROOT datasets,"
    echo "then you should set com.sun:auto-snapshot=true for ROOT datasets."
    echo ""
    read" -p Set com.sun:auto-snapshot=true for ROOT datasets? (Y or N) " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        zfs set com.sun:auto-snapshot=true $(POOL_NAME)/ROOT
    fi

    echo ""
    echo "com.sun:auto-snapshot is used by the nixos built-in services.zfs.autoSnapshot."
    echo "So if you want your nixos zfs.autoSnapshot settings to apply to your HOME dataset,"
    echo "then you should set com.sun:auto-snapshot=true for HOME dataset."
    echo ""
    read -p "Set com.sun:auto-snapshot=true for your HOME dataset? (Y or N) " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        zfs set com.sun:auto-snapshot=true ${POOL_NAME}/HOME
    fi
}

# Run the script !
# intall zfs to the livedisk, but only if it needs it.
which zfs > /dev/null 2>&1 || __bootstrapzfs

# begin disk prep interactive ()
__disk_prep

# begin pool create interactive ()
__zpool_create

# create root datasets
__datasets_create

# now that we have zfs installed to the live disk, we need to setup a zpool on
# our disk(s) where we will subsequently boot strap our own configuration.nix
# via github.
#git clone ${NIXCFG_REPO} /mnt/${NIXCFG_LOCATION}
