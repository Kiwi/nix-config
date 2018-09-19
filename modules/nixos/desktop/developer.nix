{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.developer.enable = mkEnableOption "modules.desktop.developer";
config = mkIf config.modules.desktop.developer.enable {

environment.sessionVariables = {
EDITOR = "emacsclient";
VISUAL = "emacsclient";
};

environment.systemPackages = with pkgs; [
emacs
poppler_utils poppler_gi libpng12 zlib

gitAndTools.gitFull gitAndTools.gitflow
tmux
shellcheck

gimp krita inkscape darktable

pandoc
];

};
}
