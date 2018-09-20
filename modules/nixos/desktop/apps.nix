{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.apps.enable = mkEnableOption "modules.desktop.apps";
config = mkIf config.modules.desktop.apps.enable {

# typical desktop apps
environment.systemPackages = with pkgs; [
libnotify glxinfo libva-utils vdpauinfo

chromium transmission-gtk mpv youtube-dl ffmpeg
];

};
}
