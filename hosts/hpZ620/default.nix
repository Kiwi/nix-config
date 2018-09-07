{ config, pkgs, ... }:
# only hpZ620 workstation specific settings
{
imports = [ ../../modules ];

networking.hostName = "hpZ620";

# hardware specific
boot.kernelModules = [ "microcode" "coretemp" ];
boot.kernelParams = [ "intel_iommu=on iommu=pt" "amdgpu.audio=0" "amdgpu.dc=1" ];
boot.initrd.kernelModules = [ "amdgpu" ];

# my modules
modules.general.enable = true;     # general profile for all machines
modules.desktop.enable = true;     # desktop profile for all machines
modules.libvirtd.enable = true;    # libvirtd module

# video
services.xserver.videoDrivers = [ "modesetting" ]; # might need to be amdgpu here.
services.xserver.deviceSection = ''
Option "DRI" "3"
Option "TearFree" "true"
Option "AccelMethod" "glamor"
'';

# This value determines the NixOS release with which your system is to be
# compatible, in order to avoid breaking some software such as database
# servers. You should change this only after NixOS release notes say you
# should.
system.stateVersion = "18.03"; # Did you read the comment?
}
