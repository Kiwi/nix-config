{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.apps.enable = mkEnableOption "modules.desktop.apps";
config = mkIf config.modules.desktop.apps.enable {

# typical desktop apps
environment.systemPackages = with pkgs; [
glxinfo libva-utils vdpauinfo

libnotify

chromium thunderbird transmission-gtk mpv youtube-dl ffmpeg

libreoffice-fresh texlive.combined.scheme-small
];

};
}
