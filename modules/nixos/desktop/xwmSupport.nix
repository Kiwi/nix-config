{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.xwmSupport.enable = mkEnableOption "modules.desktop.wmsupport";
config = mkIf config.modules.desktop.xwmSupport.enable {

services.xserver.displayManager = {
slim.enable = true;
slim.autoLogin = true;
slim.defaultUser = "adam";
sessionCommands = ''
/nix-config/dotfiles/bin/ghettolinker.sh
${pkgs.numlockx}/bin/numlockx
${pkgs.xlibs.setxkbmap}/bin/setxkbmap -option ctrl:swap_lalt_lctl -option caps:swapescape
${pkgs.xlibs.xset}/bin/xset r rate 250 50
${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
${pkgs.xlibs.xrdb}/bin/xrdb ~/.Xresources
'';
};

powerManagement.resumeCommands = ''
${pkgs.sudo}/bin/sudo -u adam DISPLAY=:0.0 ${pkgs.xlibs.xset}/bin/xset r rate 250 50; ${pkgs.xlibs.setxkbmap}/bin/setxkbmap -option ctrl:swap_lalt_lctl -option caps:swapescape
'';

services.compton = {
enable = true;
backend = "glx";
};

services.redshift = {
enable = false;
latitude = "43.3665";
longitude = "-124.2179";
};

environment.systemPackages = with pkgs; [
wmctrl
scrot
feh
xorg.setxkbmap xorg.xmodmap xorg.xev xorg.xrdb xorg.xset xorg.xsetroot
numlockx
xclip xsel
rxvt_unicode
];

};
}
