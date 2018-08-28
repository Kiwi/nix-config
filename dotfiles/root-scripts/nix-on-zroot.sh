#!/usr/bin/env bash

# Copyright 2018 sch@efers.org
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# A simple nix-on-zroot installer.

# instructions:
# Set variables below and then execute the script on a nixos live disk.

#################
# set variables #
#################

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

# Your personal nix-config repo to be bootstrapped
NIXCFG_REPO="git@github.com:a-schaefers/nix-config.git"

# Preferred location where we will clone the nix-config repo
NIXCFG_LOCATION="/nix-config/"

# host file to be imported from $NIXCFG_LOCATION/hosts
NIXCFG_HOST="latitudeE6430.nix"

##############################################################################
# script                                                                     #
##############################################################################

# TODO add uefi support.
if [ -d "/sys/firmware/efi/efivars" ]; then
    exit ; echo "uefi sucks"
    echo "legacy bios recommended if you can help it."
fi

echo "WARNING: The following script intends to replace all of your disk(s) \
contents with a fresh zfs-on-root NixOS installation."
echo ""
read -p "Continue? (Y or N) " -n 1 -r
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    echo "Aborted." ; exit
fi

__bootstrap_zfs() {
    sed -i '/imports/a \
boot.supportedFilesystems = [ \"zfs\" ];' \
        /etc/nixos/configuration.nix
    NEEDS_SWITCH="1"
}

__bootstrap_git() {
    sed -i '/imports/a \
 environment.systemPackages = with pkgs; [ git-minimal ];' \
        /etc/nixos/configuration.nix
    NEEDS_SWITCH="1"
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

    read -p "Remove disk signatures and other old operating system installation  remnants? (Y or N) " -n 1 -r
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

        zpool set bootfs=${POOL_NAME}/ROOT/nixos ${POOL_NAME}
    fi
}

__datasets_create() {
    mkdir /mnt/{root,home,tmp}

    # ROOT filesystem
    zfs create -o mountpoint=none -o canmount=off ${POOL_NAME}/ROOT
    zfs create -o mountpoint=legacy -o canmount=on ${POOL_NAME}/ROOT/nixos
    mount -t zfs ${POOL_NAME}/ROOT/nixos /mnt

    # HOME directory
    zfs create -o mountpoint=none -o canmount=off ${POOL_NAME}/HOME
    zfs create -o mountpoint=legacy -o canmount=on ${POOL_NAME}/HOME/homedirs
    mount -t zfs ${POOL_NAME}/HOME/homedirs /mnt/home

    # /tmp directory
    zfs create -o mountpoint=none -o canmount=off rpool/TMP
    zfs create -o mountpoint=legacy canmount=on -o sync=disabled rpool/TMP/tmp
    mount -t zfs ${POOL_NAME}/TMP/tmp /mnt/tmp

    # /NIX option
    echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    echo "Answering yes is only useful if you do not garbage collect your NixOS install \
 regularly or if you intend to keep long-term nix-store snapshots and have storage capacity concerns."
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
}

__zfs_auto_snapshot() {
    echo ""
    echo "com.sun:auto-snapshot is used by the nixos built-in services.zfs.autoSnapshot"
    echo "so if you want your nixos zfs.autoSnapshot settings to apply to ROOT datasets,"
    echo "then you should set com.sun:auto-snapshot=true for ROOT datasets."
    echo ""
    read" -p Set com.sun:auto-snapshot=true for ROOT datasets? Recommended. (Y or N) " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        zfs set com.sun:auto-snapshot=true $(POOL_NAME)/ROOT
    fi

    echo ""
    echo "com.sun:auto-snapshot is used by the nixos built-in services.zfs.autoSnapshot."
    echo "So if you want your nixos zfs.autoSnapshot settings to apply to your HOME dataset,"
    echo "then you should set com.sun:auto-snapshot=true for HOME dataset."
    echo ""
    read -p "Set com.sun:auto-snapshot=true for your HOME dataset? Recommended. (Y or N) " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        zfs set com.sun:auto-snapshot=true ${POOL_NAME}/HOME
    fi
}

# Run the script !

# intall zfs to the livedisk, but only if it needs it.
which zfs > /dev/null 2>&1 || __bootstrap_zfs

# intall git to the livedisk, but only if it needs it.
which git > /dev/null 2>&1 || __bootstrap_git

# switch if needed
if [[ ${NEEDS_SWITCH} == "1" ]]
then
    nixos-rebuild switch
fi

# begin disk prep interactive ()
__disk_prep

# begin pool create interactive ()
__zpool_create

# create a basic dataset scheme
__datasets_create

# set zfs-auto-snapshot properties interactive ()
__zfs_auto_snapshot

# generate /mnt/etc/nixos/hardware-configuration.nix
nixos-generate-config --root /mnt

# bootstrap our custom configuration
# NOTE: make sure you have a zfs nix module LOL!
git clone ${NIXCFG_REPO} /mnt/${NIXCFG_LOCATION}

cat <<EOF > /etc/nixos/configuration.nix
 { ... }:
# this can be a symlink in /etc/nixos/ or the actual file.
{ imports = [
  /etc/nixos/hardware-configuration.nix
  ${NIXCFG_LOCATION}${NIXCFG_HOST}
]; }
EOF

nixos-install

echo ""
echo "I think it's done now"
