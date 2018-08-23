{ config, lib, ... }:
 # shared configurations for all hosts
with lib;
  {
    imports = [];
    options.mine.zfsCare.enable = mkEnableOption "ZFS maintenance Profile";
    config = {
      boot.loader.grub.enable = true;
      boot.loader.grub.version = 2;
      boot.loader.grub.copyKernels = true;
      boot.supportedFilesystems = [ "zfs" ];
    } // mkIf config.mine.zfsCare.enable {
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
