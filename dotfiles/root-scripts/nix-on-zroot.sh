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
# (use full path with trailing slash.)
NIXCFG_LOCATION="/nix-config/"

# name of host file to be imported from $NIXCFG_LOCATION/hosts directory.
NIXCFG_HOST="vm.nix"

# When giving entire disks to ZFS, it is a good idea first to remove various
REMOVE_REMNANTS="1"

# Set atime to "off" or "on". Not using atime can increase SSD disk life.
ATIME="off"

# Apply com.sun:auto-snapshot=true attributes to ROOT or HOME datasets?
SNAPSHOT_ROOT="1"
SNAPSHOT_HOME="1"

##############################################################################
# script                                                                     #
##############################################################################

__uefi_or_legacy() {
    # TODO add uefi support.
    if [ -d "/sys/firmware/efi/efivars" ]; then
        exit ; echo "uefi sucks"
        echo "legacy bios recommended if you can help it."
    fi
}

__initial_warning() {
    echo "WARNING: The following script intends to replace all of your disk(s) \
contents with a fresh zfs-on-root NixOS installation."
    echo ""
    read -p "Continue? (Y or N) " -n 1 -r
    if [[ ! $REPLY =~ ^[Yy]$ ]]
    then
        echo "Aborted." ; exit
    fi
}

__bootstrap_zfs() {
    sed -i '/imports/a \
boot.supportedFilesystems = [ \"zfs\" ];' \
        /etc/nixos/configuration.nix
    NEEDS_SWITCH="1"
}

__bootstrap_git() {
    sed -i '/imports/a \
 environment.systemPackages = with pkgs; [ git ];' \
        /etc/nixos/configuration.nix
    NEEDS_SWITCH="1"
}

__disk_prep() {
    if [[ ${REMOVE_REMNANTS} == "1" ]]
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
    zpool create -f \
          -o ashift=12 \
          -O compression=lz4 \
          -O atime=${ATIME:?"Please define atime."} \
          -O relatime=on \
          -O normalization=formD \
          -O xattr=sa \
          -m none \
          -R /mnt \
          ${POOL_NAME:?"Please define pool name."} \
          ${POOL_TYPE} \
          ${POOL_DISKS:?"Please define pool disks."}

    IFS=$'\n'
    for DISK_ID in ${POOL_DISKS}
    do
        sgdisk -a1 -n2:48:2047 -t2:EF02 -c2:"BIOS boot partition" ${DISK_ID}
        partx -u ${DISK_ID}
    done
}

__datasets_create() {
    # / (root) datasets
    zfs create -o mountpoint=none -o canmount=off ${POOL_NAME}/ROOT
    zfs create -o mountpoint=legacy -o canmount=on ${POOL_NAME}/ROOT/nixos
    zpool set bootfs=${POOL_NAME}/ROOT/nixos ${POOL_NAME}
    mount -t zfs ${POOL_NAME}/ROOT/nixos /mnt

    mkdir /mnt/{home,tmp}

    # /home datasets
    zfs create -o mountpoint=none -o canmount=off ${POOL_NAME}/HOME
    zfs create -o mountpoint=legacy -o canmount=on ${POOL_NAME}/HOME/home
    mount -t zfs ${POOL_NAME}/HOME/home /mnt/home

    # /tmp datasets
    zfs create -o mountpoint=none -o canmount=off ${POOL_NAME}/TMP
    zfs create -o mountpoint=legacy canmount=on -o sync=disabled ${POOL_NAME}/TMP/tmp
    mount -t zfs ${POOL_NAME}/TMP/tmp /mnt/tmp
}

__zfs_auto_snapshot() {
    if [[ ${SNAPSHOT_HOME} == "1" ]]
    then
        zfs set com.sun:auto-snapshot=true ${POOL_NAME}/HOME
    elif [[ ${SNAPSHOT_ROOT} == "1" ]]
    then
        zfs set com.sun:auto-snapshot=true ${POOL_NAME}/ROOT
    fi
}

__switch_if_needed() {
    if [[ ${NEEDS_SWITCH} == "1" ]]
    then
        nixos-rebuild switch
    fi
}

__get_custom_nixcfg() {
    git clone ${NIXCFG_REPO} /${NIXCFG_LOCATION}
    mkdir /mnt/${NIXCFG_LOCATION}
    cp -r /${NIXCFG_LOCATION}/* /mnt/${NIXCFG_LOCATION}

    cat <<EOF > /etc/nixos/configuration.nix
{ ... }:
{ imports = [
/etc/nixos/hardware-configuration.nix
${NIXCFG_LOCATION}${NIXCFG_HOST}
]; }
EOF
}

__thank_you() {
    echo ""
    read -p "Installation finished. Reboot? (Y or N) " -n 1 -r
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        reboot
    fi

    echo "May you have a Happy Hacking." ; exit
}

# Run the script and come back later !

__uefi_or_legacy
__initial_warning
which zfs > /dev/null 2>&1 || __bootstrap_zfs
which git > /dev/null 2>&1 || __bootstrap_git
__switch_if_needed
__disk_prep
__zpool_create
__datasets_create
__zfs_auto_snapshot
nixos-generate-config --root /mnt
__get_custom_nixcfg
nixos-install
__thank_you
