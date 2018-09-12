{ config, pkgs, ... }:
# only hpZ620 workstation specific settings
{
imports = [ ../../modules ];

networking.hostName = "hpZ620";

# hardware specific
nix.buildCores = 0;
nix.maxJobs = 24;
boot.kernelPackages = pkgs.linuxPackages_4_17;
boot.kernelModules = [ "microcode" "coretemp" "vfio" "vfio_iommu_type1" "vfio_pci" "vfio_virqfd" ];
boot.extraModprobeConfig ="options vfio-pci ids=10de:0dd8,10de:0be9";
boot.kernelParams = [ "intel_iommu=on iommu=pt" "amdgpu.audio=0" "amdgpu.dc=1" ];
boot.initrd.kernelModules = [ "amdgpu" ];

# my modules
modules.general.enable = true;
modules.desktop.enable = true;
modules.libvirtd.enable = true;

# video
services.xserver.videoDrivers = [ "modesetting" ];
services.xserver.deviceSection = ''
Option "DRI" "3"
Option "TearFree" "true"
Option "AccelMethod" "glamor"
'';

services.compton = {
enable = true;
backend = "glx";
};

hardware.opengl.extraPackages = with pkgs;
[ libvdpau-va-gl vaapiVdpau ];
hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux;
[ libvdpau-va-gl vaapiVdpau ];

# environment.sessionVariables = {
# LIBVA_DRIVER_NAME = "";
# VDPAU_DRIVER = "";
# };

# This value determines the NixOS release with which your system is to be
# compatible, in order to avoid breaking some software such as database
# servers. You should change this only after NixOS release notes say you
# should.
system.stateVersion = "18.03"; # Did you read the comment?
}
