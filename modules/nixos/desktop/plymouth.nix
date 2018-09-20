{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.plymouth.enable = mkEnableOption "modules.desktop.plymouth";
config = mkIf config.modules.desktop.plymouth.enable {

# pretty load screen
boot.plymouth.enable = true;

};
}
