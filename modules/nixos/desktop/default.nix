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
];

modules.desktop = enableMultiple [
"plymouth"
"pulse"
"security"
"apps"
"developer"
# "xorg"
"opengl"
"sway"
# "exwm"
# "xwmSupport"
# "kitchenSink"
];

};
}
