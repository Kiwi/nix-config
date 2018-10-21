{ config, pkgs, lib, ... }:
with lib;
let
myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in
{
options.modules.desktop.developer.enable = mkEnableOption "modules.desktop.developer";
config = mkIf config.modules.desktop.developer.enable {

environment.sessionVariables = {
ALTERNATE_EDITOR = "";
EDITOR = "emacsclient -t";
VISUAL = "-create-frame --alternate-editor=\"\"";
};

services.emacs = {
package = myEmacs;
install = true;
enable = true;
};

environment.systemPackages = with pkgs; [
myEmacs
# for emacs pdf-tools https://github.com/politza/pdf-tools
poppler_utils poppler_gi libpng12 zlib

gitAndTools.gitFull gitAndTools.gitflow
tmux
vim
shellcheck

awscli ansible

gimp krita inkscape darktable

pandoc
];

};
}
