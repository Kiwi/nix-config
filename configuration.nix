{ config, pkgs, ... }:

let myemacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;}); in {
  imports = [
    ./hardware-configuration.nix
    ./rescue_boot.nix
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.devices = [
    "/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76820C5544"
    "/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76822C9FD0"
  ];
  boot.loader.grub.copyKernels = true;
  boot.supportedFilesystems = [ "zfs" ];
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
  networking.hostId = "007f0100";

  networking.hostName = "nixos";
  networking.networkmanager.enable = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/Los_Angeles";

  environment.sessionVariables = {
    EDITOR = "emacsclient";
    VISUAL = "emacsclient";
  };

  environment.shells = [
    "${pkgs.bash}/bin/bash"
  ];

  environment.systemPackages = with pkgs; [
    gptfdisk
    wget
    curl
    gnutls
    gnupg1compat
    pinentry
    git
    firefox
    thunderbird
    openvpn
    qbittorrent
    tmux
    mpv
    wmctrl
    xorg.xmodmap
    xorg.xset
    xorg.xsetroot
    numlockx
    scrot
    libnotify
    pkgs.acpilight
  (python36.withPackages(ps: with ps; [ certifi ]))
    pandoc
  ];

  nixpkgs.config.packageOverrides = super: {
    acpilight = pkgs.callPackage ./pkgs/acpilight.nix {};
  };

  programs.bash.enableCompletion = true;
  programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  services.openssh.enable = true;

  networking.firewall.allowedTCPPorts = [ 22 ];
  networking.firewall.allowedUDPPorts = [ 22 ];

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };

  services.compton.enable = true;
  services.compton.backend = "glx";

  services.xserver = {
    enable = true;
    layout = "us";
    libinput.enable = true;
    libinput.accelSpeed = "0.9";
    videoDrivers = [ "modesetting" ];
    useGlamor = true;
    deviceSection = ''
      Option "DRI" "3"
      Option "TearFree" "true"
      Option "AccelMethod" "glamor"
    '';

    displayManager.slim.enable = true;
    displayManager.slim.defaultUser = "adam";
    displayManager.slim.autoLogin = true;
    displayManager.sessionCommands = ''
      ${pkgs.numlockx}/bin/numlockx
      ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
      ${pkgs.xlibs.xset}/bin/xset r rate 250 50
      ${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap
      ${myemacs}/bin/emacs
    '';

    desktopManager = {
      xterm.enable = false;
      default = "none";
    };
    
  };

  fonts.fonts = with pkgs; [
    source-code-pro
    font-awesome-ttf
    powerline-fonts
  ];

  users.users.adam =
  { isNormalUser = true;
    home = "/home/adam";
    createHome = false;
    extraGroups = [ "wheel" "disk" "audio" "video" "networkmanager" "systemd-journal" ];
  };
  security.sudo.wheelNeedsPassword = false;

  powerManagement.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

}
    
  
