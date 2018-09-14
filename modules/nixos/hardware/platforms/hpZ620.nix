{ config, pkgs, lib, ... }:
# generic HP Z620 configuration
with lib;
{
options.modules.hardware.platform.hpZ620.enable = mkEnableOption "Intelgfx Profile";
config = mkIf config.modules.hardware.platform.hpZ620.enable {

nix.buildCores = 0;
nix.maxJobs = 24;
boot.kernelModules = [ "microcode" "coretemp" ];

modules.hardware.power.workstations.enable = true;
modules.hardware.amdgpu.enable = true;
};
}
