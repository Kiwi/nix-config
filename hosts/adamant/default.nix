{ config, pkgs, ... }:
# Dell Latitude E6430 - Top Level Nixfile
{
imports = [ ../../modules ];

system.stateVersion = "18.03";

# Computer name
networking.hostName = "adamant";

# use a generic latitudeE6430 profile
modules.hardware.platform.latitudeE6430.enable = true;

# Enable the modular desktop profile
modules.desktop.enable = true;

# Services
modules.services.libvirtd.enable = true;

}
