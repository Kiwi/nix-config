{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.apps.enable = mkEnableOption "modules.desktop.apps";
config = mkIf config.modules.desktop.apps.enable {

# typical desktop apps
environment.systemPackages = with pkgs; [
glxinfo libva-utils vdpauinfo

transmission-gtk youtube-dl ffmpeg

libreoffice-fresh texlive.combined.scheme-small
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
