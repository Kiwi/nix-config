# Themelios configuration.sh example
POOL_NAME="zroot"
POOL_TYPE="mirror"    # E.g. change to "" for single disk, or maybe "raidz1" with 3 disks. :)

# Separate each disk using new lines. (Note: using /dev/disk/by-id is also preferable.)
POOL_DISKS="/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76820C5544
/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76822C9FD0"

# Datasets to be set with com.sun:auto-snapshot=true (Separate with new lines.)
AUTO_SNAPSHOT="${POOL_NAME}/HOME
${POOL_NAME}/ROOT"

ATIME="off"           # Set to "on" or "off" (recommended "off" for SSD.)
USE_ZSWAP="false"     # Creates a swap zvol
ZSWAP_SIZE="4G"
SGDISK_CLEAR="true"   # Use sgdisk --clear
WIPEFS_ALL="true"     # Use wipefs --all
ZERO_DISKS="false"    # Use dd if=/dev/zero ...

# Your top-level configuration.nix file to be bootstrapped-- (use the relative path from the project_root.)
TOP_LEVEL_NIXFILE="hosts/latitudeE6430"

# Directory name of to clone your git-remote in "/" (root). # May be anything, but do not use slashes.
# This is intended to be the directory to operate the nix installation from.
NIXCFG_DIR="nix-config"

# If true, mount /nix outside of the / (root) dataset.
# Setting this to true would trade-off the ability to use zfs boot environments for extra disk space.
# If you use nix.gc.automatic, then this should not be much of an issue. Recommended "false".
NIXDIR_NOROOT="false"

# Creates /etc/nixos/themelios-zfs.nix with sensible settings
THEMELIOS_ZFS="true"

# Enable ZFS_CARE Options? (Only enable this if THEMELIOS_ZFS="true" also.)
THEMELIOS_ZFS_CARE="true"

####################
# ZFS_CARE Options #
####################

# Auto Scrubs
care_autoScrub="true"

# Auto Snapshots
care_autoSnapshot_enabled="true"
care_autoSnapshot_frequent="8"   # Take a snapshot every 15 minutes and keep 8 in rotation
care_autoSnapshot_hourly="0"
care_autoSnapshot_daily="7"      # Take a daily snapshot and keep 7 in rotation
care_autoSnapshot_weekly="0"
care_autoSnapshot_monthly="0"

# Auto Garbage Collection
care_gc_automatic="true"
care_gc_dates="daily"
care_gc_options="--delete-older-than 7d"

# Auto /tmp clean
care_cleanTmpDir="true"

###################
# Overlay Section #
###################

# If set, Themelios will source them if the files exist alongside configuration.sh
POOL_OVERLAY_FILE="overlay-pool"         # Override __zpool_create()
DATASETS_OVERLAY_FILE="overlay-datasets" # Override __datasets_create()
