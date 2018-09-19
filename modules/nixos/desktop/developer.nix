{ config, pkgs, lib, ... }:
with lib;
let
myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in
{
options.modules.desktop.developer.enable = mkEnableOption "modules.desktop.developer";
config = mkIf config.modules.desktop.developer.enable {

environment.sessionVariables = {
EDITOR = "emacsclient";
VISUAL = "emacsclient";
};

environment.systemPackages = with pkgs; [
myEmacs
poppler_utils poppler_gi libpng12 zlib

gitAndTools.gitFull gitAndTools.gitflow
tmux
vim
shellcheck

gimp krita inkscape darktable

pandoc
];

};
}
