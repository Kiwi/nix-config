{ config, pkgs, lib, ... }:
# generic desktop power settings
with lib;
{
options.modules.hardware.power.desktops.enable = mkEnableOption "modules.hardware.power.desktops";
config = mkIf config.modules.hardware.power.desktops.enable {

powerManagement = {
enable = true;
cpuFreqGovernor = "ondemand";
};

};
}
