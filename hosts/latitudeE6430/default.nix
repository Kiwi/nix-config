{ config, pkgs, ... }:
# dell latitude E6430 machine-hardware unique and machine-purpose unique settings
{
imports = [ ../../modules ];

# Machine-Hardware Specific Configuration #

# set specific name of computer
networking.hostName = "adamant";

# machine-specific threads (This can vary from machine-to-machine even of the same model number.)
nix.maxJobs = 4;

# use a generic latitudeE6430 profile
modules.hardware.platform.latitudeE6430.enable = true;

# Machine-Purpose Specific Configuration # (The purpose this machine is to be used for.)

system.stateVersion = "18.03";

# Setup my desktop developer profiles.
modules.desktop.enable = true;
modules.desktop.wmsupport.enable = true;
modules.desktop.exwm.enable = true;
modules.desktop.developer.enable = true;
modules.services.libvirtd.enable = true;
}
