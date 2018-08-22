{ config, pkgs, ... }:
# all $HOME and user settngs

# TODO, consider installing and doing more things user-locally
# and consider exploring home-manager features more beyond symlink management.

let
  # location of dotfiles
  adamDotfiles = "/nixcfg/dotfiles";

  # create some helper cli commands

  # clone all of my favorite repos
  cloneRepos = pkgs.writeScriptBin "mynixos-cloneRepos" ''
    mkdir ~/repos/
    cd ~/repos/
    git clone git@github.com:cooslug/cooslug.github.io.git
    git clone git@github.com:a-schaefers/a-schaefers.github.io.git
    git clone git@github.com:a-schaefers/grubbe-mkconfig.git
    git clone git@github.com:TemptorSent/Funtools.git
    git clone git@github.com:apoptosis/episteme.git
    git clone git@github.com:dustinlacewell/emacs-nougat.git
    git clone git@github.com:bbatsov/prelude.git
  '';

  # remove $HOME cruft with a list of exceptions to keep.
    cleanHome = pkgs.writeScriptBin "mynixos-cleanHome" ''
      cd ~/
      find -name "*" | egrep -v \
      "bash_history|ssh|gnupg|gpg|chromium|qBittorrent|emacs|slime|repos|Documents|Downloads" \
      | xargs rm -f
      find . -type d -empty -delete
      sudo systemctl restart home-manager-adam || exit
      pkill emacs
    '';
in
{

  imports = [
    # make home-manager available https://nixos.wiki/wiki/Home_Manager
    "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
  ];

  # make the helper bash functions available
  environment.systemPackages = with pkgs; [
    cloneRepos
    cleanHome
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
    home.file.".gitconfig".source = "${adamDotfiles}/.gitconfig";
    home.file.".bash_profile".source = "${adamDotfiles}/.bash_profile";
    home.file.".bashrc".source = "${adamDotfiles}/.bashrc";
    home.file."/bin".source = "${adamDotfiles}/bin";
    home.file."/.config/mimi/mime.conf".source = "${adamDotfiles}/.config/mimi/mime.conf";
    home.file."/.config/mpv/mpv.conf".source = "${adamDotfiles}/.config/mpv/mpv.conf";
    home.file.".inputrc".source = "${adamDotfiles}/.inputrc";
    home.file.".mailcap".source = "${adamDotfiles}/.mailcap";
    home.file.".Xmodmap".source = "${adamDotfiles}/.Xmodmap";
  };

}
