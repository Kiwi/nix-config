{ config, pkgs, lib, ... }:
with lib;
let
mynixos-symlinks = pkgs.writeScriptBin "mynixos-symlinks" ''
#!${pkgs.stdenv.shell}
# TODO this should just be automated.

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

mkdir -p ~/.config/{mpv,mimi,gtk-3.0,dunst}
ln -sf /nix-config/dotfiles/.config/gtk-3.0/settings.ini ~/.config/gtk-3.0/settings.ini
ln -sf /nix-config/dotfiles/.config/mpv/mpv.conf ~/.config/mpv/mpv.conf
ln -sf /nix-config/dotfiles/.config/mimi/mime.conf ~/.config/mimi/mime.conf
ln -sf /nix-config/dotfiles/.config/dunst/dunstrc ~/.config/dunst/dunstrc

ln -sfn /nix-config/dotfiles/Private/ssh ~/.ssh
ln -sf /nix-config/dotfiles/Private/gnupg ~/.gnupg
ln -sf /nix-config/dotfiles/Private/authinfo.gpg ~/.authinfo.gpg
ln -sf /nix-config/dotfiles/Private/passwd.gpg ~/.passwd.gpg

sudo ln -sf /nix-config/dotfiles/.bashrc /root/.bashrc
sudo ln -sf /nix-config/dotfiles/.bash_profile /root/.bash_profile
sudo ln -sf /nix-config/dotfiles/.inputrc /root/.inputrc
sudo ln -sfn /nix-config/dotfiles/Private/mullvad /root/
'';

in {
options.modules.desktop.wmsupport.enable = mkEnableOption "modules.desktop.wmsupport";
config = mkIf config.modules.desktop.wmsupport.enable {

services.xserver.displayManager = {
lightdm.enable = true;
sessionCommands = ''
${mynixos-symlinks}/bin/mynixos-symlinks
${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap
${pkgs.xlibs.xset}/bin/xset r rate 250 50
${pkgs.numlockx}/bin/numlockx
${pkgs.numlockx}/bin/dunst &
'';
};

services.compton = {
enable = true;
backend = "glx";
};

environment.systemPackages = with pkgs; [
mynixos-symlinks

lxappearance
numix-cursor-theme
arc-theme
arc-icon-theme

libnotify dunst
wmctrl
scrot
xorg.xmodmap xorg.xev xorg.xrdb xorg.xset xorg.xsetroot
numlockx
xclip xsel
];

};
}
