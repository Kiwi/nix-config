{ config, lib, ... }:
with lib;
  {
    imports = [];
    options.mine.zfsCare.enable = mkEnableOption "ZFS maintenance Profile";
    config = {
      boot.loader.grub.copyKernels = true; # recommended if /boot/grub resides on zfs.
      boot.supportedFilesystems = [ "zfs" ]; # all hosts get zfs support
    } // mkIf config.mine.zfsCare.enable {
      # set mine.zfsCare.enable = true; for the following options:
      boot.zfs.forceImportAll = false;
      boot.zfs.forceImportRoot = false;
      services.zfs.autoScrub.enable = true;
      services.zfs.autoSnapshot = {
        enable = true;
        frequent = 8;
        hourly = 0;
        daily = 7;
        weekly = 0;
        monthly = 0;
      };
      nix.gc.automatic = true;
      nix.gc.dates = "daily";
      nix.gc.options = "--delete-older-than 7d";
      boot.cleanTmpDir = true;
    };
  }
