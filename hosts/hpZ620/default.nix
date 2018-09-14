{ config, pkgs, ... }:
# only hpZ620 workstation specific settings
{
imports = [ ../../modules ];

system.stateVersion = "18.03";
networking.hostName = "hpZ620";

# machine specific
powerManagement.enable = true;
nix.buildCores = 0;
nix.maxJobs = 24;
boot.kernelPackages = pkgs.linuxPackages_4_17;
boot.kernelModules = [ "microcode" "coretemp" "vfio" "vfio_iommu_type1" "vfio_pci" "vfio_virqfd" ];
boot.extraModprobeConfig ="options vfio-pci ids=10de:0dd8,10de:0be9";
boot.kernelParams = [ "intel_iommu=on iommu=pt" "amdgpu.audio=0" "amdgpu.dc=1" ];
boot.initrd.kernelModules = [ "amdgpu" ];

# my modules
modules.workstation.enable = true;
modules.dev.enable = true;
modules.amdgpu.enable = true;
modules.exwm.enable = true;
modules.dotfiles.enable = true;
modules.libvirtd.enable = true;

}
