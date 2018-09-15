{ config, pkgs, ... }:
# Dell Latitude E6430 Machine-unique Hardware & Software Configuration
{
imports = [
../../modules
./pci-passthrough.nix
./quirks.nix
];

system.stateVersion = "18.03";

# Computer name
networking.hostName = "adamant";

# use a generic latitudeE6430 profile
modules.hardware.platform.latitudeE6430.enable = true;

# Enable the modular desktop profile
modules.desktop.enable = true;

# Services
modules.services.libvirtd.enable = true; # Virtual machines
}
