{ config, pkgs, lib, ... }:
# Enable for all machines with real hardware (e.g. NOT virtual machines.)
with lib;
{
options.modules.hardware.metal.enable = mkEnableOption "modules.hardware";
config = mkIf config.modules.hardware.metal.enable {

nix.buildCores = 0;

boot.kernelModules = [ "coretemp" ];

services.acpid.enable = true;

services.smartd.enable = true;

environment.systemPackages = with pkgs; [
pmutils acpi acpid lm_sensors smartmontools
];

};
}
