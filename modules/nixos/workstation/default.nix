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

# Xorg, lightdm
services.xserver = {
enable = true;
layout = "us";
useGlamor = true;
displayManager.lightdm.enable = true;
displayManager.sessionCommands = ''
${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap
${pkgs.xlibs.xset}/bin/xset r rate 250 50
${pkgs.numlockx}/bin/numlockx
${pkgs.dunst}/bin/dunst &
'';};

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
libnotify dunst

firefox chromium qbittorrent mpv youtube-dl pavucontrol
];

};
}
