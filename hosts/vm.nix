{ config, pkgs, ... }:
# zfs test vm settings
 {
   imports = [
     ../modules/nixos/nixos-zfs.nix
     ../modules/nixos/nixos-etc.nix
   ];

   boot.kernelModules = [ "microcode" ];
   boot.kernelParams = [ "" ];

   # put devices all on one line if using nix-on-zroot.sh (for now...)
   boot.loader.grub.devices = [ "/dev/sda" "/dev/sdb" ];

   networking.hostName = "nixvm";

   mine.zfs.enable = true;

   # Bootstrap Block - Set one variable per line beginning with #|
     #|POOL_NAME=zroot
     #|POOL_TYPE=single
     #|POOL_DISKS=/dev/sda
     #|POOL_HOSTID=random
     #|NIXCFG_LOCATION=/nix-config/
     #|REMOVE_REMNANTS=true
     #|ATIME=false
     #|SNAPSHOT_ROOT=true
     #|SNAPSHOT_HOME=true
     #|USE_ZSWAP=false
     #|ZSWAP_SIZE=4G
 }
