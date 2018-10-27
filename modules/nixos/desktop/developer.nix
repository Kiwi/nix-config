{ config, pkgs, lib, ... }:
with lib;
let
myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in
{
options.modules.desktop.developer.enable = mkEnableOption "modules.desktop.developer";
config = mkIf config.modules.desktop.developer.enable {

environment.systemPackages = with pkgs; [
gitAndTools.gitFull gitAndTools.gitflow
shellcheck
awscli ansible
gimp krita inkscape darktable
pandoc aspell libreoffice-fresh texlive.combined.scheme-small
];

};
}
