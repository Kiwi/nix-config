{ config, pkgs, lib, ... }:
with lib;
let
in {
options.modules.wmsupport.enable = mkEnableOption "WM Support Profile";
config = mkIf config.modules.wmsupport.enable {

environment.systemPackages = with pkgs; [
lxappearance
numix-cursor-theme
arc-theme
arc-icon-theme

wmctrl
scrot
xorg.xmodmap xorg.xev xorg.xrdb xorg.xset xorg.xsetroot
numlockx
xclip xsel
];

};
}
