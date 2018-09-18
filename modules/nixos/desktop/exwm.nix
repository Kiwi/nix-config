{ config, pkgs, lib, ... }:
with lib;
let
#myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in {
options.modules.desktop.exwm.enable = mkEnableOption "modules.desktop.exwm";
config = mkIf config.modules.desktop.exwm.enable {

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

environment.systemPackages = with pkgs; [
emacs
poppler_utils poppler_gi libpng12 zlib
];

};
}
