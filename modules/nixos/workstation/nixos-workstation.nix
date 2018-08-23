{ config, pkgs, lib, ... }:

with lib;
  let
    myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
  in
{
  options.mine.workstation.enable = mkEnableOption "Workstation Profile";

  config = mkIf config.mine.workstation.enable {

    # misc services I don't really use

    # services.samba.enable = true;
    # services.locate.enable = true;
    # services.printing.enable = true;
    # services.avahi.enable = true;
    # services.avahi.nssmdns = true;

    services.smartd.enable = true;

    # pulseaudio
    sound.enable = true;
    hardware.pulseaudio.enable = true;

    # mesa
    hardware.opengl = {
      driSupport = true;
      driSupport32Bit = true;
    };

    # Xorg, Slim, Emacs
    services.xserver = {
      enable = true;
      layout = "us";
      useGlamor = true;
      displayManager.slim.enable = true;
      displayManager.slim.autoLogin = true;
      displayManager.slim.defaultUser = "adam";
      displayManager.sessionCommands = ''
        ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
        ${pkgs.xlibs.xset}/bin/xset r rate 250 50
        ${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap
        ${pkgs.numlockx}/bin/numlockx
        ${pkgs.dunst}/bin/dunst &
      '';
      desktopManager = {
        xterm.enable = false;
        default = "emacs";
        session = [ {
          manage = "desktop";
          name = "emacs";
          start = ''
            ${myEmacs}/bin/emacs &
            waitPID=$!
          '';}];};};

    environment.sessionVariables = {
      EDITOR = "emacsclient";
      VISUAL = "emacsclient";
    };

    # some basic xft fonts
    fonts.fonts = with pkgs; [
      dejavu_fonts
      source-code-pro ];

    environment.systemPackages = with pkgs; [
      myEmacs

      # desktop support applications
      wmctrl
      scrot
      xorg.xmodmap xorg.xev xorg.xrdb xorg.xset xorg.xsetroot
      numlockx
      xclip xsel
      libnotify dunst

      # desktop apps
      pandoc
      chromium qbittorrent mpv pavucontrol
      gimp kdenlive darktable krita inkscape

      # security
      (python36.withPackages(ps: with ps; [ certifi ]))
      gnutls gnupg gnupg1compat pinentry
      openvpn
    ];

    programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
    };
}
