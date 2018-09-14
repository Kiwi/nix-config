{ config, pkgs, ... }:
# only dell latitude E6430 specific settings
{
imports = [ ../../modules ];

system.stateVersion = "18.03";
networking.hostName = "latitudeE6430";

modules.hardware.platform.latitudeE6430.enable = true;
modules.workstation.enable = true;
modules.workstation.wmsupport.enable = true;
modules.workstation.exwm.enable = true;
modules.workstation.developer.enable = true;
modules.services.libvirtd.enable = true;
}
