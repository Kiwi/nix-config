{ config, pkgs, lib, ... }:
with lib;
let
# myEmacs = (pkgs.emacs.override {withGTK3=false; withGTK2=false; withX=true;});
in {
options.modules.dev.enable = mkEnableOption "dev Profile";
config = mkIf config.modules.dev.enable {

environment.systemPackages = with pkgs; [
emacs
#myEmacs
aspell aspellDicts.en
gitAndTools.gitFull gitAndTools.gitflow
tmux
shellcheck

gimp kdenlive frei0r darktable krita inkscape

pandoc
];

};
}
