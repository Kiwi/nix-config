{ config, pkgs, lib, ... }:
with lib;
let
in {
options.modules.workstation.enable = mkEnableOption "Workstation Profile";
config = mkIf config.modules.workstation.enable {

# splash screen.
boot.plymouth.enable = true;

# misc services I don't really use

# services.samba.enable = true;
# services.locate.enable = true;
# services.printing.enable = true;
# services.avahi.enable = true;
# services.avahi.nssmdns = true;

services.acpid.enable = true;
services.smartd.enable = true;

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

environment.systemPackages = with pkgs; [
pmutils acpi acpid lm_sensors smartmontools

poppler_utils poppler_gi libpng12 zlib

numix-cursor-theme
arc-theme
arc-icon-theme

glxinfo
wmctrl
scrot
xorg.xmodmap xorg.xev xorg.xrdb xorg.xset xorg.xsetroot libva-utils vdpauinfo
numlockx
xclip xsel

firefox chromium qbittorrent mpv ffmpeg youtube-dl pavucontrol
];

};
}
