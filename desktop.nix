{ config, pkgs, ... }:

let
  myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
  iconTheme = pkgs.breeze-icons.out;
  themeEnv = ''
    # QT: remove local user overrides (for determinism, causes hard to find bugs)
    rm -f ~/.config/Trolltech.conf

    # GTK3: remove local user overrides (for determinisim, causes hard to find bugs)
    rm -f ~/.config/gtk-3.0/settings.ini

    # GTK3: add breeze theme to search path for themes
    # (currently, we need to use gnome-breeze because the GTK3 version of kde5.breeze is broken)
    export XDG_DATA_DIRS="${pkgs.gnome-breeze}/share:$XDG_DATA_DIRS"

    # GTK3: add /etc/xdg/gtk-3.0 to search path for settings.ini
    # We use /etc/xdg/gtk-3.0/settings.ini to set the icon and theme name for GTK 3
    export XDG_CONFIG_DIRS="/etc/xdg:$XDG_CONFIG_DIRS"

    # GTK2 theme + icon theme
    export GTK2_RC_FILES=${pkgs.writeText "iconrc" ''gtk-icon-theme-name="breeze"''}:${pkgs.breeze-gtk}/share/themes/Breeze/gtk-2.0/gtkrc:$GTK2_RC_FILES

    # SVG loader for pixbuf (needed for GTK svg icon themes)
    export GDK_PIXBUF_MODULE_FILE=$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)

    # QT5: convince it to use our preferred style
    export QT_STYLE_OVERRIDE=breeze
  '';

in {

  imports = [];


  services.compton = {
    enable = true;
    backend = "glx";
    extraOptions = ''
      paint-on-overlay = true;
      vsync = opengl-mswc;
    '';};

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

    displayManager.job.logToJournal = true;
    displayManager.slim.enable = true;
    displayManager.slim.defaultUser = "adam";
    displayManager.slim.autoLogin = true;
    displayManager.sessionCommands = ''
      ${pkgs.numlockx}/bin/numlockx
      ${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
      ${pkgs.xlibs.xset}/bin/xset r rate 250 50
      ${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap
      ${pkgs.dunst}/bin/dunst &
      ${myEmacs}/bin/emacs
    '';

    desktopManager = {
      xterm.enable = false;
      default = "none";
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

  environment.shells = [
    "${pkgs.bash}/bin/bash"
  ];

  environment.sessionVariables = {
    EDITOR = "emacsclient";
    VISUAL = "emacsclient";
    PATH="$HOME/bin:$HOME/.local/bin:$PATH";
  };

  environment.extraInit = ''
    ${themeEnv}

    # these are the defaults, but some applications are buggy so we set them
    # here anyway
    export XDG_CONFIG_HOME=$HOME/.config
    export XDG_DATA_HOME=$HOME/.local/share
    export XDG_CACHE_HOME=$HOME/.cache
  '';

  # QT5 global theme
  environment.etc."xdg/Trolltech.conf" = {
    text = ''
      [Qt]
      style=Breeze
    '';
    mode = "444";
  };

  # GTK3 global theme (widget and icon theme)
  environment.etc."xdg/gtk-3.0/settings.ini" = {
    text = ''
      [Settings]
      gtk-icon-theme-name=breeze
      gtk-theme-name=Breeze-gtk
    '';
    mode = "444";
  };

  environment.systemPackages = with pkgs; [
    # Qt theme
    breeze-qt5

    # Icons (Main)
    iconTheme

    # Icons (Fallback)
    gnome3.adwaita-icon-theme
    hicolor_icon_theme
  ];

  # (This is good for desktops, is set with nixos KDE by default)
    environment.pathsToLink = [ "/share" ];

}
