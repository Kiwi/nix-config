{ config, pkgs, lib, ... }:
with lib;
  {
    imports = [  ];

    options.mine.zfs.enable = mkEnableOption "ZFS Profile";
    config = mkIf config.mine.zfs.enable {
      boot.kernelParams = "elevator=noop boot.shell_on_fail";
      boot.supportedFilesystems = [ "zfs" ];
      boot.loader.grub.copyKernels = true; # recommended if /boot/grub resides on zfs.
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
