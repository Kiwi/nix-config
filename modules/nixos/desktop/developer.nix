{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.developer.enable = mkEnableOption "modules.desktop.developer";
config = mkIf config.modules.desktop.developer.enable {

environment.systemPackages = with pkgs; [
gitAndTools.gitFull gitAndTools.gitflow
tmux
shellcheck

gimp krita inkscape darktable

pandoc
];

};
}
