{ config, pkgs, ... }:
 {
   imports = [
     ./hosts/latitudeE6430.nix
     # ./hosts/hpZ620.nix
     ./hosts/modules/rescue_boot.nix
     ./hosts/modules/desktop.nix
     ./hosts/modules/home.nix
   ];

   boot.kernelModules = [ "coretemp" "kvm-intel" "microcode" ];
   boot.loader.grub.enable = true;
   boot.loader.grub.version = 2;
   boot.loader.grub.copyKernels = true;
   boot.supportedFilesystems = [ "zfs" ];
   boot.zfs.forceImportAll = false;
   boot.zfs.forceImportRoot = false;
   services.zfs.autoScrub.enable = true;
   services.zfs.autoSnapshot = {
     enable = true;
     frequent = 8;
     hourly = 0;
     daily = 21;
     weekly = 0;
     monthly = 0;
   };
   services.smartd.enable = true;
   nix.gc.automatic = true;
   nix.gc.dates = "weekly";
   nix.gc.options = "--delete-older-than 30d";
   boot.cleanTmpDir = true;

   i18n = {
     consoleFont = "Lat2-Terminus16";
     consoleKeyMap = "us";
     defaultLocale = "en_US.UTF-8";
   };

   time.timeZone = "America/Los_Angeles";

   environment.systemPackages = with pkgs; [
     # misc utilities
     wget curl inetutils nix-prefetch-scripts gptfdisk pmutils psmisc which file
     binutils bc utillinuxCurses exfat dosfstools patchutils moreutils unzip zip
     pciutils lshw usbutils

     # performance / monitoring
     lm_sensors
     htop iotop powertop
     ltrace strace linuxPackages.perf
     smartmontools

     # misc

     (python36.withPackages(ps: with ps; [ certifi ]))
     gnutls gnupg gnupg1compat pinentry
   ];

   programs.bash.enableCompletion = true;
   environment.shells = [
     "${pkgs.bash}/bin/bash"
   ];

   networking.networkmanager.enable = true;

   networking.firewall.allowPing = true;
   networking.firewall.allowedTCPPorts = [ 22 ];
   networking.firewall.allowedUDPPorts = [ 22 ];
   networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
   networking.hostName = "nix";

   services.openssh.enable = true;

   powerManagement.enable = true;

   nix.allowedUsers = [ "root" "@wheel" ];
   nix.trustedUsers = [ "root" "@wheel" ];

   # This value determines the NixOS release with which your system is to be
     # compatible, in order to avoid breaking some software such as database
   # servers. You should change this only after NixOS release notes say you
   # should.
   system.stateVersion = "18.03"; # Did you read the comment?

 }
