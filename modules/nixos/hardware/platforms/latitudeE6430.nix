{ config, pkgs, lib, ... }:
# generic dell latitude E6430 configuration
with lib;
{
options.modules.hardware.platform.latitudeE6430.enable = mkEnableOption "hardware.platform.latitudeE6430";
config = mkIf config.modules.hardware.platform.latitudeE6430.enable {

nix.buildCores = 0;
nix.maxJobs = 4;
boot.kernelModules = [ "microcode" "coretemp" ];
boot.kernelParams = [ "i915.enable_fbc=1" ];
boot.initrd.kernelModules = [ "i915" ];

modules.hardware.power.laptops.enable = true;
modules.hardware.intelgfx.enable = true;
modules.hardware.touchpad.enable = true;
services.xserver.libinput.accelSpeed = "0.9";
};
}
