{ config, pkgs, lib, ... }:
# generic laptop power savings settings.
with lib;
{
options.modules.hardware.power.laptops.enable = mkEnableOption "modules.hardware.power.laptops";
config = mkIf config.modules.hardware.power.laptops.enable {

powerManagement.enable = true;
services.tlp.enable = true;

};
}
