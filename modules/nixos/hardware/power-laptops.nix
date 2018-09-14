{ config, pkgs, lib, ... }:
# generic laptop power savings settings.
with lib;
{
options.modules.hardware.power.laptops.enable = mkEnableOption "Intelgfx Profile";
config = mkIf config.modules.hardware.power.laptops.enable {

powerManagement.enable = true;
services.tlp.enable = true;

};
}
