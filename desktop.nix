{ config, pkgs, ... }:

let
  myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in {
  imports = [];

  services.dbus.socketActivated = true;
  services.tlp.enable = true;
  services.samba.enable = true;
  services.locate.enable = true;
  services.printing.enable = true;
  services.avahi.enable = true;
  services.avahi.nssmdns = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  networking.networkmanager.enable = true;

  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };

  services.compton = {
    enable = true;
    backend = "glx";
  };

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

    displayManager.sddm.enable = true;
    displayManager.sddm.autoLogin.enable = true;
    displayManager.sddm.autoLogin.user = "adam"; 
    displayManager.sddm.autoNumlock = true;
    displayManager.sessionCommands = ''
      ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
      ${pkgs.xlibs.xset}/bin/xset r rate 250 50
      ${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap
      ${pkgs.dunst}/bin/dunst &
    '';

    desktopManager = {
      xterm.enable = false;
      default = "emacs";
      session = [ {
        manage = "desktop";
        name = "emacs";
        start = ''
          ${myEmacs}/bin/emacs
        '';
      } ];
    };

  };

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    dejavu_fonts
    source-code-pro
    font-awesome-ttf
    powerline-fonts
  ];

  environment.sessionVariables = {
    EDITOR = "emacsclient";
    VISUAL = "emacsclient";
    PATH="$HOME/bin:$HOME/.local/bin:$PATH";
  };

  environment.systemPackages = with pkgs; [
    tmux
    gitAndTools.gitFull gitAndTools.gitflow
    pandoc
    pavucontrol
    chromium firefox thunderbird qbittorrent
    gimp
    kdenlive
    darktable
    krita
    openvpn
    virtmanager
    mpv
    wmctrl scrot
    pkgs.acpilight
    xorg.xmodmap xorg.xev xorg.xset xorg.xsetroot xclip xsel
    libnotify dunst
  ];

nixpkgs.config.packageOverrides = super: {
  acpilight = pkgs.callPackage ./pkgs/acpilight.nix {};
};

}
