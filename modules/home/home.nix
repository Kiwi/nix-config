{ config, pkgs, lib, ... }:
# all $HOME and user configurations
with lib;
  let
    # workaround https://github.com/NixOS/nix/issues/2405
    extraPkgs = import <nixpkgs> { config={}; overlays=[]; };
    homeManager = extraPkgs.fetchFromGitHub {
      owner = "rycee";
    repo = "home-manager";
    rev = "9fe6fa7f44d8672f1824620192fe585edee14fcc";
      sha256 = "qw8mmvd4229vr42cvxixd1i2w7qmyy1c";
    };

adamDotfiles = "/nix-config/dotfiles";

  cloneRepos = pkgs.writeScriptBin "mynixos-cloneRepos" ''
    #!${pkgs.stdenv.shell}
    # clone all of my favorite repos
    mkdir ~/repos/
    cd ~/repos/
    git clone git@github.com:cooslug/cooslug.github.io.git
    git clone git@github.com:a-schaefers/a-schaefers.github.io.git
    git clone git@github.com:a-schaefers/grubbe-mkconfig.git
    git clone git@github.com:TemptorSent/Funtools.git
    git clone git@github.com:apoptosis/episteme.git
    git clone git@github.com:dustinlacewell/emacs-nougat.git
    git clone git@github.com:bbatsov/prelude.git
    git clone git@github.com:a-schaefers/themelios.git
  '';

  cleanHome = pkgs.writeScriptBin "mynixos-cleanHome" ''
    #!${pkgs.stdenv.shell}
    # yes i know this is rediculous, but what the hay.
    cd ~/
    find -name "*" | egrep -v \
    "bash_history|ssh|gnupg|gpg|mozilla|qBittorrent|emacs|slime|repos|Documents|Downloads" \
    | xargs rm -f
    find . -type d -empty -delete
    systemctl restart home-manager-adam || exit
    pkill emacs
  '';

  in
  {
    imports = [
      # make home-manager available https://nixos.wiki/wiki/Home_Manager
      "${homeManager}/nixos"
    ];

    options.mine.home.enable = mkEnableOption "Home Profile";
    config = mkIf config.mine.home.enable {

      # system-wide developer environment packages
      environment.systemPackages = with pkgs; [
        cloneRepos
        cleanHome
        gitAndTools.gitFull gitAndTools.gitflow
        tmux
      ];

      # setup users
      users.users.adam =
        { isNormalUser = true;
          home = "/home/adam";
          createHome = true;
          extraGroups = [ "wheel" "disk" "audio" "video" "systemd-journal"
            "networkmanager" "libvirtd" ];
        };
      security.sudo.wheelNeedsPassword = false;

      # home-manager for "adam" section
      home-manager.users.adam = {
        # create symlinks to my dotfiles (recreated every time the home-manager-adam service restarts)
        home.file.".emacs.d/init.el".source = "${adamDotfiles}/.emacs.d/init.el";
        home.file.".emacs.d/lisp.d".source = "${adamDotfiles}/.emacs.d/lisp.d";
        home.file.".Xmodmap".source = "${adamDotfiles}/.Xmodmap";
        home.file.".gitconfig".source = "${adamDotfiles}/.gitconfig";
        home.file.".bash_profile".source = "${adamDotfiles}/.bash_profile";
        home.file.".bashrc".source = "${adamDotfiles}/.bashrc";
        home.file.".inputrc".source = "${adamDotfiles}/.inputrc";
        home.file."/bin".source = "${adamDotfiles}/bin";
        home.file."/.config/mimi/mime.conf".source = "${adamDotfiles}/.config/mimi/mime.conf";
        home.file.".mailcap".source = "${adamDotfiles}/.mailcap";
        home.file."/.config/mpv/mpv.conf".source = "${adamDotfiles}/.config/mpv/mpv.conf";
      };

      home-manager.users.root = {
        home.file.".bash_profile".source = "${adamDotfiles}/.bash_profile";
        home.file.".bashrc".source = "${adamDotfiles}/.bashrc";
        home.file.".inputrc".source = "${adamDotfiles}/.inputrc";
        home.file."/bin".source = "${adamDotfiles}/root-scripts";
      };

    };
  }
