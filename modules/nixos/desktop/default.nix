{ config, pkgs, lib, ... }:
with lib;
with import ../../../util;
{
options.modules.desktop.enable = mkEnableOption "modules.desktop";
config = mkIf config.modules.desktop.enable {

# misc desktop-ish services I don't really use

boot.plymouth.enable = true;
# services.samba.enable = true;
# services.locate.enable = true;
# services.printing.enable = true;
# services.avahi.enable = true;
# services.avahi.nssmdns = true;

# pulseaudio
sound.enable = true;
hardware.pulseaudio.enable = true;

# some basic xft fonts
fonts.fonts = with pkgs; [
dejavu_fonts
source-code-pro
];

# typical desktop apps
environment.systemPackages = with pkgs; [
glxinfo libva-utils vdpauinfo

firefox qbittorrent mpv ffmpeg youtube-dl pavucontrol
];

modules.desktop = enableMultiple [
"sway"
"opengl"
# "exwm"
# "xwmSupport"
# "xorg"
"security"
"developer"
];

};
}
