{ config, pkgs, ... }:
# zfs test vm settings
 {
   imports = [ ../modules ];

   boot.kernelModules = [ "microcode" ];
   boot.kernelParams = [ "" ];

   # put devices all on one line if using nix-on-zroot.sh (for now...)
   boot.loader.grub.devices = [ "/dev/sda" "/dev/sdb" ];

   #networking.hostName = "nixvm";
   #networking.hostId = "007f0100";

   mine.zfs.enable = true;
 }
