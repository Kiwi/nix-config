{ config, pkgs, lib, ... }:
with lib;
with import ../../../util;
{
options.modules.desktop.enable = mkEnableOption "modules.desktop";
config = mkIf config.modules.desktop.enable {

sound.enable = true;

# some basic xft fonts
fonts.fonts = with pkgs; [
dejavu_fonts
source-code-pro
];

modules.desktop = enableMultiple [
"plymouth"
"sway"
"opengl"
"pulse"
"security"
"apps"
"developer"
# "xorg"
# "exwm"
# "xwmSupport"
# "kitchenSink"
];

};
}
