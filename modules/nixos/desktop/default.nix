{ config, pkgs, lib, ... }:
with lib;
with import ../../../util;
{
options.modules.desktop.enable = mkEnableOption "modules.desktop";
config = mkIf config.modules.desktop.enable {

# splash screen.
boot.plymouth.enable = true;

# misc desktop-ish services I don't really use

# services.samba.enable = true;
# services.locate.enable = true;
# services.printing.enable = true;
# services.avahi.enable = true;
# services.avahi.nssmdns = true;

# pulseaudio
sound.enable = true;
hardware.pulseaudio.enable = true;

# Xorg
services.xserver = {
enable = true;
layout = "us";
useGlamor = true;
};

# mesa
hardware.opengl = {
driSupport = true;
driSupport32Bit = true;
};

# some basic xft fonts
fonts.fonts = with pkgs; [
dejavu_fonts
source-code-pro
];

# typical desktop apps
environment.systemPackages = with pkgs; [
glxinfo libva-utils vdpauinfo

firefox thunderbird qbittorrent mpv ffmpeg youtube-dl pavucontrol
];

modules.desktop = enableMultiple [
"exwm"
"wmsupport"
"security"
"developer"
];

};
}
