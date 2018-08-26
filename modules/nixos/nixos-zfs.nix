{ config, lib, ... }:
with lib;
  {
    imports = [];

    options.mine.zfs.enable = mkEnableOption "ZFS Profile";
    config = mkIf config.mine.zfs.enable {

      # some basic zfs recommended settings
      boot.kernelParams = [ "elevator=noop" "boot.shell_on_fail" ];
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

      # Use gc.automatic with zfs-auto-snapshot to keep disk space under control.
      nix.gc.automatic = true;
      nix.gc.dates = "daily";
      nix.gc.options = "--delete-older-than 7d";

      # clean /tmp automatically on boot
      boot.cleanTmpDir = true;

      # TODO add script here for com...

    };

  }
