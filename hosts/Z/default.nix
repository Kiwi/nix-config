{ config, pkgs, ... }:
#  HP Z620 machine-hardware unique and machine-purpose unique settings
{
imports = [ ../../modules ];

# Machine-Hardware Specific Configuration #

# use a generic hpZ620 profile
modules.hardware.platform.hpZ620.enable = true;

# use a generic amdgpu profile
modules.hardware.amdgpu.enable = true;

# set specific name of computer
networking.hostName = "Z";

# machine-specific threads (This can vary from machine-to-machine even of the same model number.)
nix.maxJobs = 24;
nix.buildCores = 0;

# pci passthrough for the quadro2000
# additionally amdgpu.audio parameter unique for the amd rx460
boot.kernelPackages = pkgs.linuxPackages_4_17; # use until -gt 4.15 becomes nixos default.
boot.kernelModules = [ "vfio" "vfio_iommu_type1" "vfio_pci" "vfio_virqfd" ];
boot.extraModprobeConfig ="options vfio-pci ids=10de:0dd8,10de:0be9";
boot.kernelParams = [ "intel_iommu=on iommu=pt" "amdgpu.audio=0" ];

# Machine-Software Specific Configuration # (The purpose this machine is to be used for.)

system.stateVersion = "18.03";

# Setup my desktop developer profiles.
modules.desktop.enable = true;
modules.desktop.security.enable = true;
modules.desktop.wmsupport.enable = true;
modules.desktop.exwm.enable = true;
modules.desktop.developer.enable = true;
modules.services.libvirtd.enable = true;
}
