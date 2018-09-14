{ config, pkgs, lib, ... }:
with lib;
{
options.modules.exwm.enable = mkEnableOption "Exwm Profile";
config = mkIf config.modules.exwm.enable {

services.xserver.desktopManager = {
xterm.enable = false;
default = "emacs";
session = [ {
manage = "desktop";
name = "emacs";
start = ''
${pkgs.emacs}/bin/emacs &
waitPID=$!
'';}];};

environment.sessionVariables = {
EDITOR = "emacsclient";
VISUAL = "emacsclient";
_JAVA_AWT_WM_NONREPARENTING = "1";
};

};
}
