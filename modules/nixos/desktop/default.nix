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
# "kitchenSink"
"plymouth"
"pulse"
"security"
"apps"
"developer"
"xorg"
"xwmSupport"
"opengl"
# "sway" # comment xwmSupport and xorg if using this one.
# "exwm"
"ratpoison"
];

};
}
