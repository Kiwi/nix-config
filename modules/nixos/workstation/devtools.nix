{ config, pkgs, lib, ... }:
with lib;
let
# myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in {
options.modules.devtools.enable = mkEnableOption "dev Profile";
config = mkIf config.modules.devtools.enable {

environment.systemPackages = with pkgs; [
emacs
#myEmacs
poppler_utils poppler_gi libpng12 zlib
aspell aspellDicts.en

gitAndTools.gitFull gitAndTools.gitflow
tmux
shellcheck

gimp kdenlive frei0r darktable krita inkscape

pandoc
];

};
}
