{ config, pkgs, ... }:
# HP Z620 Machine-unique Hardware & Software Configuration
{
imports = [
../../modules
./pci-passthrough.nix
./quirks.nix
];

system.stateVersion = "18.03";

# Computer name
networking.hostName = "Z";

# Use a generic hpZ620 profile
modules.hardware.platform.hpZ620.enable = true;

# Use a generic amdgpu profile
modules.hardware.amdgpu.enable = true;

# Enable the modular desktop profile
modules.desktop.enable = true;

# Services
modules.services.libvirtd.enable = true; # Virtual machines
}
