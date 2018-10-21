{ config, pkgs, lib, ... }:
with lib;
with import ../../../util;
{
options.modules.desktop.enable = mkEnableOption "modules.desktop";
config = mkIf config.modules.desktop.enable {

# alsa
sound.enable = true;

# some basic fonts
fonts.fonts = with pkgs; [
dejavu_fonts
source-code-pro
noto-fonts
noto-fonts-cjk
noto-fonts-emoji
liberation_ttf
];

modules.desktop = enableMultiple [
"pulse"
"security"
"apps"
"developer"
"xorg"
"xwmSupport"
"opengl"
"ratpoison"
];

};
}
