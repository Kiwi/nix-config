{ config, pkgs, lib, ... }:
with lib;
let
# myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in {
options.modules.desktop.developer.enable = mkEnableOption "modules.desktop.developer";
config = mkIf config.modules.desktop.developer.enable {

environment.systemPackages = with pkgs; [
emacs
#myEmacs
poppler_utils poppler_gi libpng12 zlib
aspell aspellDicts.en

gitAndTools.gitFull gitAndTools.gitflow
tmux
shellcheck

gimp krita inkscape darktable

pandoc
];

};
}
