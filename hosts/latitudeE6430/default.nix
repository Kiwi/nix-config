{ config, pkgs, ... }:
# only dell latitude E6430 specific settings
{
imports = [ ../../modules ];

system.stateVersion = "18.03";
networking.hostName = "latitudeE6430";

modules.hardware.platform.latitudeE6430.enable = true;
modules.desktop.enable = true;
modules.desktop.wmsupport.enable = true;
modules.desktop.exwm.enable = true;
modules.desktop.developer.enable = true;
modules.services.libvirtd.enable = true;
}
