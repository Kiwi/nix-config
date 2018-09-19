{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.wmsupport.enable = mkEnableOption "modules.desktop.wmsupport";
config = mkIf config.modules.desktop.wmsupport.enable {

services.xserver.displayManager = {
lightdm.enable = true;
sessionCommands = ''
/nix-config/dotfiles/bin/ghettolinker.sh
[[ -f ~/.Xmodmap ]] && ${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap
[[ -f ~/.Xresources ]] && ${pkgs.xlibs.xrdb}/bin/xrdb ~/.Xresources
${pkgs.xlibs.xset}/bin/xset r rate 250 50
${pkgs.numlockx}/bin/numlockx
${pkgs.numlockx}/bin/dunst &
${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
'';
};

services.compton = {
enable = true;
backend = "glx";
};

environment.systemPackages = with pkgs; [
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
