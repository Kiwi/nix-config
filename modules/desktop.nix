{ config, pkgs, ... }:

let
  myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
  adamDotfiles = "/nixcfg/dotfiles";
  cloneRepos = pkgs.writeScriptBin "mynixos-cloneRepos" ''
    mkdir ~/repos/
    cd ~/repos/
    git clone git@github.com:cooslug/cooslug.github.io.git
    git clone git@github.com:a-schaefers/a-schaefers.github.io.git
    git clone git@github.com:a-schaefers/grubbe-mkconfig.git
    git clone git@github.com:apoptosis/episteme.git
    git clone git@github.com:dustinlacewell/emacs-nougat.git
    git clone git@github.com:bbatsov/prelude.git
    git clone git@github.com:syl20bnr/spacemacs.git
    git clone git@github.com:purcell/emacs.d.git
    git clone git@github.com:emacs-tw/awesome-emacs.git
  '';
  cleanHome = pkgs.writeScriptBin "mynixos-cleanHome" ''
    # remove $HOME cruft with a list of exceptions to keep.
    cd ~/
    find -name "*" | egrep -v \
    "bash_history|ssh|gnupg|gpg|chromium|qBittorrent|emacs|slime|repos|Pictures|Documents|Downloads" \
    | xargs rm -f
    find . -type d -empty -delete
    # re-create symlinks
    sudo systemctl restart home-manager-adam || exit
    # log out and back in again
    pkill emacs
  '';
in {
  imports = [
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
  ];

  home-manager.users.adam = {
    home.file.".emacs.d/init.el".source = "${adamDotfiles}/.emacs.d/init.el";
    home.file.".emacs.d/lisp.d".source = "${adamDotfiles}/.emacs.d/lisp.d";
    home.file.".gitconfig".source = "${adamDotfiles}/.gitconfig";
    home.file.".bash_profile".source = "${adamDotfiles}/.bash_profile";
    home.file.".bashrc".source = "${adamDotfiles}/.bashrc";
    home.file."/bin".source = "${adamDotfiles}/bin";
    home.file."/.config/mimi/mime.conf".source = "${adamDotfiles}/.config/mimi/mime.conf";
    home.file."/.config/mpv/mpv.conf".source = "${adamDotfiles}/.config/mpv/mpv.conf";
    home.file.".inputrc".source = "${adamDotfiles}/.inputrc";
    home.file.".mailcap".source = "${adamDotfiles}/.mailcap";
    home.file.".Xmodmap".source = "${adamDotfiles}/.Xmodmap";

    home.file.".gtkrc-2.0".source = "${adamDotfiles}/.gtkrc-2.0";
    home.file."/.config/.gtk-3.0/settings.ini".source = "${adamDotfiles}/.config/.gtk-3.0/settings.ini";
    home.file."/.icons/default/index.theme".source = "${adamDotfiles}/.icons/default/index.theme";
    home.file.".Xresources".source = "${adamDotfiles}/.Xresources";
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
          ${pkgs.emacs}/bin/emacs &
          # ${myEmacs}/bin/emacs &
          waitPID=$!
        '';}];};};

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
    PATH = "$HOME/bin:$HOME/.local/bin:$PATH";
  };

  environment.systemPackages = with pkgs; [
    numix-cursor-theme
    numix-gtk-theme
    numix-icon-theme
    lxappearance
    cloneRepos
    cleanHome
    emacs
    # myEmacs
    tmux
    gitAndTools.gitFull gitAndTools.gitflow
    pandoc
    chromium qbittorrent gimp kdenlive darktable krita virtmanager pavucontrol
    openvpn
    mpv wmctrl scrot
    xorg.xmodmap xorg.xev xorg.xrdb xorg.xset xorg.xsetroot numlockx xclip xsel
    pkgs.acpilight
    libnotify dunst
  ];

  nixpkgs.config.packageOverrides = super: {
    acpilight = pkgs.callPackage ./acpilight.nix {};
  };

}