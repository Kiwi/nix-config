{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.xmonad.enable = mkEnableOption "modules.desktop.xmonad";
config = mkIf config.modules.desktop.xmonad.enable {

services.xserver = {
windowManager.xmonad = {
enable = true;
enableContribAndExtras = true;
extraPackages = haskellPackages: [
haskellPackages.xmonad-contrib
haskellPackages.xmonad-extras
haskellPackages.xmonad
];
};
windowManager.default = "xmonad";
};

};
}
