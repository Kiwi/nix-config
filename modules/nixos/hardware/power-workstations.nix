{ config, pkgs, lib, ... }:
# generic workstation power settings
with lib;
{
options.modules.hardware.power.workstations.enable = mkEnableOption "modules.hardware.power.workstations";
config = mkIf config.modules.hardware.power.workstations.enable {

powerManagement = {
enable = true;
cpuFreqGovernor = "ondemand";
};

};
}
