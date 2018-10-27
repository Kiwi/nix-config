{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.apps.enable = mkEnableOption "modules.desktop.apps";
config = mkIf config.modules.desktop.apps.enable {

# typical desktop apps
environment.systemPackages = with pkgs; [
glxinfo libva-utils vdpauinfo

numix-cursor-theme arc-theme arc-icon-theme lxappearance

transmission-gtk youtube-dl ffmpeg
];

programs.firejail = {
enable = true;
wrappedBinaries = {
chromium = "${lib.getBin pkgs.chromium}/bin/chromium";
mpv = "${lib.getBin pkgs.mpv}/bin/mpv";
};
};


};
}
