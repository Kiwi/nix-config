{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.ratpoison.enable = mkEnableOption "modules.desktop.ratpoison";
config = mkIf config.modules.desktop.ratpoison.enable {

services.xserver.desktopManager.xterm.enable = false;
services.xserver.windowManager.ratpoison.enable = true;

};
}
