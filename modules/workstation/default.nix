{ config, pkgs, lib, ... }:

with lib;

{
  options.mine.workstation.enable = mkEnableOption "Workstation Profile";

  config = mkIf config.mine.workstation.enable {
    # misc services I don't really use

    # services.samba.enable = true;
    # services.locate.enable = true;
    # services.printing.enable = true;
    # services.avahi.enable = true;
    # services.avahi.nssmdns = true;

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

      # crypto / security
      (python36.withPackages(ps: with ps; [ certifi ]))
      gnutls gnupg gnupg1compat pinentry
      openvpn
    ];

    mine.workstation = {
      # pulseaudio
      sound.enable = true;
      hardware.pulseaudio.enable = true;

      # mesa
      hardware.opengl = {
        driSupport = true;
        driSupport32Bit = true;
      };

      # Xorg, Slim, Emacs
      xserver.enable = true;

    };


  };
}
