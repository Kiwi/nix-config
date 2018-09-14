{ config, pkgs, ... }:
# only dell latitude E6430 specific settings
{
imports = [ ../../modules ];

system.stateVersion = "18.03";
networking.hostName = "latitudeE6430";

powerManagement.enable = true;
nix.buildCores = 0;
nix.maxJobs = 4;
boot.kernelModules = [ "microcode" "coretemp" ];
boot.kernelParams = [ "i915.enable_fbc=1" ];
boot.initrd.kernelModules = [ "i915" ];

# my modules
modules.workstation.enable = true;
modules.devtools.enable = true;
modules.intelgfx.enable = true;
modules.exwm.enable = true;
modules.wmsupport.enable = true;
modules.dotfiles.enable = true;
modules.libvirtd.enable = true;

# boost laptop power savings
services.tlp.enable = true;

# touchpad
services.xserver.libinput.enable = true;
services.xserver.libinput.accelSpeed = "0.9";
}
