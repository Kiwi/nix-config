# Themelios configuration.sh example
POOL_NAME="zroot"
POOL_TYPE="raidz1"    # May also be set to "mirror". Leave empty "" for single.

# use one disk per line here!
POOL_DISKS="/dev/sda
/dev/sdb
/dev/sdc"

SGDISK_CLEAR="true"   # Use sgdisk --clear
WIPEFS_ALL="true"     # wipefs --all
ZERO_DISKS="false"    # uses dd if=/dev/zero ...
ATIME="off"           # recommended "off" for SSD disks.
SNAPSHOT_ROOT="true"  # Sets the value of com.sun:auto-snapshot
SNAPSHOT_HOME="true"
USE_ZSWAP="false"     # Creates a swap zvol
ZSWAP_SIZE="4G"

THEMELIOS_ZFS="true"  # Creates /etc/nixos/themelios-zfs.nix with sensible settings

# Your top-level configuration.nix file relative path from the project_root.
# e.g. for the file project_root/hosts/hpZ620/default.nix use the following:
TOP_LEVEL_NIXFILE="hosts/hpZ620/default.nix"

# Directory name of <git-remote> in "/" (root). Do not use slashes.
NIXCFG_DIR="nix-config"

# Setting this to true would trade-off the ability to use zfs boot environments for extra disk space.
# OTOH, if you garbage collect often, this should not be much of an issue. (Recommended false.)
NIXDIR_NOROOT="false" # mount /nix outside of the / (root) dataset.
