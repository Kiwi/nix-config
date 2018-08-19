{ config, pkgs, ... }:
 {
   imports = [
     ./hardware-configuration.nix
     ./rescue_boot.nix
     ./desktop.nix
   ];

   boot.cleanTmpDir = true;
   boot.loader.grub.enable = true;
   boot.loader.grub.version = 2;
   boot.loader.grub.devices = [
     "/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76820C5544"
     "/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76822C9FD0"
   ];
   boot.loader.grub.copyKernels = true;
   boot.supportedFilesystems = [ "zfs" ];
   services.smartd.enable = true;
   services.zfs.autoScrub.enable = true;
   services.zfs.autoSnapshot = {
     enable = true;
     frequent = 8;
     hourly = 0;
     daily = 21;
     weekly = 0;
     monthly = 0;
   };
   boot.zfs.forceImportAll = false;
   boot.zfs.forceImportRoot = false;
   boot.kernelParams = [ "elevator=noop intel_iommu=on iommu=pt boot.shell_on_fail" ];
   boot.kernelModules = [ "coretemp" "kvm-intel" ];
   networking.hostId = "007f0100";

   networking.hostName = "nixos";
   networking.networkmanager.enable = true;

   i18n = {
     consoleFont = "Lat2-Terminus16";
     consoleKeyMap = "us";
     defaultLocale = "en_US.UTF-8";
   };

   time.timeZone = "America/Los_Angeles";

   environment.systemPackages = with pkgs; [
     man man-pages posix_man_pages stdman

     gptfdisk pmutils psmisc which file binutils bc utillinuxCurses exfat
     dosfstools patchutils moreutils unzip zip

     lm_sensors htop iotop powertop

     ltrace strace linuxPackages.perf

     smartmontools pciutils lshw usbutils

     tmux wget curl inetutils nix-prefetch-scripts

     (python36.withPackages(ps: with ps; [ certifi ]))

     gnutls gnupg gnupg1compat pinentry

     gitAndTools.gitFull gitAndTools.gitflow
     pandoc

     chromium firefox thunderbird qbittorrent
     openvpn
     virtmanager

     mpv
     wmctrl scrot
     pkgs.acpilight
     xorg.xmodmap xorg.xset xorg.xsetroot xclip xsel numlockx 
     libnotify 
   ];

   nixpkgs.config.packageOverrides = super: {
     acpilight = pkgs.callPackage ./pkgs/acpilight.nix {};
   };

   programs.bash.enableCompletion = true;
   programs.mtr.enable = true;
   programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

   networking.firewall.allowPing = true;
   networking.firewall.allowedTCPPorts = [ 22 ];
   networking.firewall.allowedUDPPorts = [ 22 ];

   sound.enable = true;
   hardware.pulseaudio.enable = true;

   hardware.opengl = {
     driSupport = true;
     driSupport32Bit = true;
   };

   services.tlp.enable = true;
   services.openssh.enable = true;
   services.samba.enable = true;
   services.locate.enable = true;
   services.printing.enable = true;
   services.avahi.enable = true;
   services.avahi.nssmdns = true;
   services.dnsmasq.enable = true;
   services.dbus.socketActivated = true;

   virtualisation.libvirtd.enable = true;
   environment.variables.LIBVIRT_DEFAULT_URI = "qemu:///system";

   users.users.adam =
     { isNormalUser = true;
       home = "/home/adam";
       createHome = false;
       extraGroups = [ "wheel" "disk" "audio" "video" "systemd-journal"
         "networkmanager" "libvirtd" ];
     };
   security.sudo.wheelNeedsPassword = false;

   services.logind.extraConfig = ''
     HandleLidSwitchDocked=suspend
   '';

   security.polkit.enable = true;
   security.polkit.extraConfig = ''
     polkit.addRule(function(action, subject) {
     var YES = polkit.Result.YES;
     var permission = {
     "org.freedesktop.udisks2.filesystem-mount": YES,
     "org.freedesktop.udisks2.filesystem-mount-system": YES,
     "org.freedesktop.udisks2.eject-media": YES
     };
     return permission[action.id];
     });
   '';

powerManagement.enable = true;

  # This value determines the NixOS release with which your system is to be
    # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

 }


