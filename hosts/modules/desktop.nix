{ config, pkgs, ... }:
 # all system-wide, machine independant desktop settings
let
  # build emacs without gtk
  myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in {
  imports = [];

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # misc services I don't really use

  # services.samba.enable = true;
  # services.locate.enable = true;
  # services.printing.enable = true;
  # services.avahi.enable = true;
  # services.avahi.nssmdns = true;

  # pulseaudio
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # mesa
  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };

  # Xorg, Slim and Exwm
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

  fonts.fonts = with pkgs; [
    dejavu_fonts
    source-code-pro
  ];

  environment.sessionVariables = {
    EDITOR = "emacsclient";
    VISUAL = "emacsclient";
  };

  environment.systemPackages = with pkgs; [
    # dev / misc
    myEmacs
    gitAndTools.gitFull gitAndTools.gitflow
    tmux
    openvpn

    # desktop support applications
    wmctrl
    scrot
    xorg.xmodmap xorg.xev xorg.xrdb xorg.xset xorg.xsetroot
    numlockx
    xclip xsel
    pkgs.acpilight
    libnotify dunst

    # desktop apps
    pandoc
    chromium qbittorrent mpv pavucontrol
    gimp kdenlive darktable krita inkscape
  ];

}
