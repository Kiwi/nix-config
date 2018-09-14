{ config, pkgs, ... }:
# only hpZ620 machine-specific settings
{
imports = [ ../../modules ];

system.stateVersion = "18.03";
networking.hostName = "hpZ620";

# machine-specific pci passthrough for the quadro2000
# additionally machine-specific specific graphics settings for amd rx460
boot.kernelPackages = pkgs.linuxPackages_4_17; # use until -gt 4.15 becomes nixos default.
boot.kernelModules = [ "vfio" "vfio_iommu_type1" "vfio_pci" "vfio_virqfd" ];
boot.extraModprobeConfig ="options vfio-pci ids=10de:0dd8,10de:0be9";
boot.kernelParams = [ "intel_iommu=on iommu=pt" "amdgpu.audio=0" "amdgpu.dc=1" ];

modules.hardware.platform.hpZ620.enable = true;
modules.desktop.enable = true;
modules.desktop.wmsupport.enable = true;
modules.desktop.exwm.enable = true;
modules.desktop.developer.enable = true;
modules.services.libvirtd.enable = true;
}
