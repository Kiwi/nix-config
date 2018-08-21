{ config, pkgs, ... }:

let
  myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
  adamDotfiles = "/home/adam/repos/dotfiles";
in {
  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
  ];

  home-manager.users.adam = {
    home.file.".gitconfig".source = "${adamDotfiles}/.gitconfig";
    home.file.".bash_profile".source = "${adamDotfiles}/.bash_profile";
    home.file.".bashrc".source = "${adamDotfiles}/.bashrc";
    home.file."/bin".source = "${adamDotfiles}/bin";
    home.file.".config/mimi/mime.conf".source = "${adamDotfiles}/.config/mimi/mime.conf";
    home.file.".inputrc".source = "${adamDotfiles}/.inputrc";
    home.file.".mailcap".source = "${adamDotfiles}/.mailcap";
    home.file.".Xmodmap".source = "${adamDotfiles}/.Xmodmap";
  };

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

    displayManager.slim.enable = true;
    displayManager.slim.autoLogin = false;
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
          ${myEmacs}/bin/emacs
        '';}];};
  };

  fonts.fonts = with pkgs; [
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
    xorg.xmodmap xorg.xev xorg.xset xorg.xsetroot numlockx xclip xsel
    libnotify dunst
  ];

  nixpkgs.config.packageOverrides = super: {
    acpilight = pkgs.callPackage ./pkgs/acpilight.nix {};
  };

}
