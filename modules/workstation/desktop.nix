{ config, pkgs, ... }:
  # system-wide, machine independant desktop configurations
let
  # build emacs without gtk
  myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in {
  imports = [];

  # Xorg, Slim and Emacs
  # (Exwm & Emacs pkgs are maintained in /nix-config/dotfiles/.emacs.d using straight.el)
  services.xserver = {
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

  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

}
