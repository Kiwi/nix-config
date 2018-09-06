{ config, pkgs, lib, ... }:

with lib;
let

adamSymlinks = pkgs.writeScriptBin "mynixos-symlinks" ''
#!${pkgs.stdenv.shell}
mkdir ~/.emacs.d
mkdir -p ~/.config/{mpv,mimi}
ln -sf /nix-config/dotfiles/bin ~/bin
ln -sf /nix-config/dotfiles/.emacs.d/init.el ~/.emacs.d/init.el
ln -sf /nix-config/dotfiles/.emacs.d/lisp.d ~/.emacs.d/lisp.d
ln -sf /nix-config/dotfiles/.Xmodmap ~/.Xmodmap
ln -sf /nix-config/dotfiles/.gitconfig ~/.gitconfig
ln -sf /nix-config/dotfiles/.bash_profile ~/.bash_profile
ln -sf /nix-config/dotfiles/.bashrc ~/.bashrc
ln -sf /nix-config/dotfiles/.inputrc ~/.inputrc
ln -sf /nix-config/dotfiles/.config/mimi/mime.conf ~/.config/mimi/mime.conf
ln -sf /nix-config/dotfiles/.mailcap ~/.mailcap
ln -sf /nix-config/dotfiles/.config/mpv/mpv.conf ~/.config/mpv/mpv.conf
'';

cloneRepos = pkgs.writeScriptBin "mynixos-cloneRepos" ''
#!${pkgs.stdenv.shell}
# clone all of my favorite repos
mkdir ~/repos/
cd ~/repos/
git clone git@github.com:cooslug/cooslug.github.io.git
git clone git@github.com:a-schaefers/themelios.git
git clone git@github.com:a-schaefers/grubbe-mkconfig.git
git clone git@github.com:apoptosis/episteme.git
git clone git@github.com:a-schaefers/a-schaefers.github.io.git
git clone git@github.com:TemptorSent/Funtools.git
git clone git@github.com:bbatsov/prelude.git
'';

cleanHome = pkgs.writeScriptBin "mynixos-cleanHome" ''
#!${pkgs.stdenv.shell}
# Yes, I know this is rediculous, but what the hay guys :)
cd ~/
find -name "*" | egrep -v \
"bash_history|ssh|gnupg|gpg|mozilla|qBittorrent|emacs|slime|repos|Documents|Downloads" \
| xargs rm -f
find . -type d -empty -delete
pkill emacs
'';
in
{
imports = [];

options.mine.home.enable = mkEnableOption "Home Profile";
config = mkIf config.mine.home.enable {

# system-wide developer environment packages
environment.systemPackages = with pkgs; [
cloneRepos
cleanHome
adamSymlinks
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

};
}
