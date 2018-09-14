{ config, pkgs, lib, ... }:
# generic HP Z620 configuration
with lib;
{
options.modules.hardware.platform.hpZ620.enable = mkEnableOption "hardware.platform.hpZ620";
config = mkIf config.modules.hardware.platform.hpZ620.enable {

modules.hardware.enable = true;
modules.hardware.power.desktops.enable = true;

};
}
