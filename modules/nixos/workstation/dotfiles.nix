{ config, pkgs, lib, ... }:
with lib;
let

# create my symlinks
mynixos-symlinks = pkgs.writeScriptBin "mynixos-symlinks" ''
#!${pkgs.stdenv.shell}
ln -sfn /nix-config/dotfiles/bin ~/bin
ln -sf /nix-config/dotfiles/.bash_profile ~/.bash_profile
ln -sf /nix-config/dotfiles/.bashrc ~/.bashrc
ln -sf /nix-config/dotfiles/.inputrc ~/.inputrc
ln -sf /nix-config/dotfiles/.gitconfig ~/.gitconfig
ln -sf /nix-config/dotfiles/.mailcap ~/.mailcap
ln -sf /nix-config/dotfiles/.Xmodmap ~/.Xmodmap
ln -sf /nix-config/dotfiles/.gtkrc-2.0 ~/.gtkrc-2.0
ln -sf /nix-config/dotfiles/.Xresources ~/.Xresources

mkdir ~/.emacs.d
ln -sfn /nix-config/dotfiles/.emacs.d/lisp.d ~/.emacs.d/lisp.d
ln -sf /nix-config/dotfiles/.emacs.d/init.el ~/.emacs.d/init.el

mkdir -p ~/.config/{mpv,mimi,gtk-3.0}
ln -sf /nix-config/dotfiles/.config/gtk-3.0/settings.ini ~/.config/gtk-3.0/settings.ini
ln -sf /nix-config/dotfiles/.config/mpv/mpv.conf ~/.config/mpv/mpv.conf
ln -sf /nix-config/dotfiles/.config/mimi/mime.conf ~/.config/mimi/mime.conf

ln -sfn /nix-config/dotfiles/Private/ssh ~/.ssh
ln -sf /nix-config/dotfiles/Private/gnupg ~/.gnupg
ln -sf /nix-config/dotfiles/Private/authinfo.gpg ~/.authinfo.gpg
ln -sf /nix-config/dotfiles/Private/passwd.gpg ~/.passwd.gpg

sudo ln -sf /nix-config/dotfiles/.bashrc /root/.bashrc
sudo ln -sf /nix-config/dotfiles/.bash_profile /root/.bash_profile
sudo ln -sf /nix-config/dotfiles/.inputrc /root/.inputrc
sudo ln -sfn /nix-config/dotfiles/Private/mullvad /root/
'';

# clone my repos
mynixos-repos = pkgs.writeScriptBin "mynixos-repos" ''
#!${pkgs.stdenv.shell}
mkdir ~/repos/
cd ~/repos/
git clone git@github.com:a-schaefers/a-schaefers.github.io.git
git clone git@github.com:cooslug/cooslug.github.io.git
git clone git@github.com:a-schaefers/themelios.git
git clone git@github.com:apoptosis/episteme.git
git clone git@github.com:bbatsov/prelude.git
'';

in {
options.modules.dotfiles.enable = mkEnableOption "Dotfiles Profile";
config = mkIf config.modules.dotfiles.enable {

environment.systemPackages = with pkgs; [
mynixos-repos
mynixos-symlinks
];

};
}
