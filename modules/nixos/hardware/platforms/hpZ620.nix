{ config, pkgs, lib, ... }:
# generic HP Z620 configuration
with lib;
{
options.modules.hardware.platform.hpZ620.enable = mkEnableOption "hardware.platform.hpZ620";
config = mkIf config.modules.hardware.platform.hpZ620.enable {

nix.buildCores = 0;
boot.kernelModules = [ "microcode" "coretemp" ];
modules.hardware.power.desktops.enable = true;

};
}
