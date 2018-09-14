{ config, pkgs, lib, ... }:
# generic workstation power settings
with lib;
{
options.modules.hardware.power.workstations.enable = mkEnableOption "Intelgfx Profile";
config = mkIf config.modules.hardware.power.workstations.enable {

powerManagement = {
enable = true;
cpuFreqGovernor = "ondemand";
};

};
}
