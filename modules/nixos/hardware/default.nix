{ config, pkgs, lib, ... }:
# Enable for all machines with real hardware (e.g. NOT virtual machines.)
with lib;
{
options.modules.hardware.enable = mkEnableOption "modules.hardware";
config = mkIf config.modules.hardware.enable {

boot.kernelModules = [ "microcode" "coretemp" ];

services.acpid.enable = true;

services.smartd.enable = true;

environment.systemPackages = with pkgs; [
pmutils acpi acpid lm_sensors smartmontools
];

};
}
