{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.sway.enable = mkEnableOption "modules.desktop.sway";
config = mkIf config.modules.desktop.sway.enable {

users.users.adam.extraGroups = [ "sway" ];
services.mingetty.autologinUser = "adam";

programs.sway = {
enable = true;
extraSessionCommands =  ''
/nix-config/dotfiles/bin/ghettolinker.sh
export XKB_DEFAULT_OPTIONS=ctrl:swap_lalt_lctl,caps:swapescape
export WLC_REPEAT_DELAY=250
export WLC_REPEAT_RATE=35
export _JAVA_AWT_WM_NONREPARENTING=1
'';
extraPackages = with pkgs; [ dmenu xwayland rxvt_unicode xorg.setxkbmap xclip xsel ];
};

};
}
