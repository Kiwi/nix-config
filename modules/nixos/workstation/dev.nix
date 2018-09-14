{ config, pkgs, lib, ... }:
with lib;
let
in {
options.modules.dev.enable = mkEnableOption "dev Profile";
config = mkIf config.modules.dev.enable {

environment.systemPackages = with pkgs; [
emacs
aspell aspellDicts.en
gitAndTools.gitFull gitAndTools.gitflow
tmux
shellcheck

gimp kdenlive darktable krita inkscape

pandoc
];

};
}
