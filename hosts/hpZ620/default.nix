{ config, pkgs, ... }:
# only hpZ620 specific settings
{
imports = [ ../../modules ];

system.stateVersion = "18.03";
networking.hostName = "hpZ620";

powerManagement.enable = true;
nix.buildCores = 0;
nix.maxJobs = 24;
boot.kernelPackages = pkgs.linuxPackages_4_17;
boot.kernelModules = [ "microcode" "coretemp" "vfio" "vfio_iommu_type1" "vfio_pci" "vfio_virqfd" ];
boot.extraModprobeConfig ="options vfio-pci ids=10de:0dd8,10de:0be9";
boot.kernelParams = [ "intel_iommu=on iommu=pt" "amdgpu.audio=0" "amdgpu.dc=1" ];
boot.initrd.kernelModules = [ "amdgpu" ];

#TODO not done yet.
# my modules
modules.hardware.amdgpu.enable = true;
modules.workstation.enable = true;
modules.workstation.wmsupport.enable = true;
modules.workstation.exwm.enable = true;
modules.workstation.developer.enable = true;
modules.services.libvirtd.enable = true;
}
