{ config, pkgs, ... }:
# HP Z620 Machine-unique Hardware & Software Configuration
{
imports = [ ../../modules ];

system.stateVersion = "18.03";

# Computer name
networking.hostName = "Z";

# Use a generic hpZ620 profile
modules.hardware.platform.hpZ620.enable = true;

# Use a generic amdgpu profile
modules.hardware.amdgpu.enable = true;

# pci passthrough for the quadro2000
# additionally amdgpu.audio parameter unique for the amd rx460
boot.kernelPackages = pkgs.linuxPackages_4_17; # use until -gt 4.15 becomes nixos default.
boot.kernelModules = [ "vfio" "vfio_iommu_type1" "vfio_pci" "vfio_virqfd" ];
boot.extraModprobeConfig ="options vfio-pci ids=10de:0dd8,10de:0be9";
boot.kernelParams = [ "intel_iommu=on iommu=pt" "amdgpu.audio=0" ];

# Desktop
modules.desktop.enable = true;           # Generic Xorg, Mesa, Browser apps
modules.desktop.wmsupport.enable = true; # Better support for Window Managers (No Desktop Environment)
modules.desktop.exwm.enable = true;      # Use Emacs as a window manager
modules.desktop.developer.enable = true; # My developer profile
modules.desktop.security.enable = true;  # My security settings for desktops

# Services
modules.services.libvirtd.enable = true; # Virtual machines
}
