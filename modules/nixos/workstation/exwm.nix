{ config, pkgs, lib, ... }:
with lib;
{
options.modules.workstation.exwm.enable = mkEnableOption "Exwm Profile";
config = mkIf config.modules.workstation.exwm.enable {

services.xserver.desktopManager = {
xterm.enable = false;
default = "emacs";
session = [ {
manage = "desktop";
name = "emacs";
start = ''
${pkgs.emacs}/bin/emacs &
waitPID=$!
'';}];
};

environment.sessionVariables = {
EDITOR = "emacsclient";
VISUAL = "emacsclient";
_JAVA_AWT_WM_NONREPARENTING = "1";
};

};
}
