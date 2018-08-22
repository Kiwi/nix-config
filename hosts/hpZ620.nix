{ config, pkgs, ... }:
 {
   imports = [  ];
   boot.kernelParams = [ "elevator=noop intel_iommu=on iommu=pt boot.shell_on_fail i915.enable_fbc=1" ];

   boot.kernelModules = [ "coretemp" "kvm-intel" "microcode" ];
   boot.kernelParams = [ "elevator=noop iommu=pt intel_iommu=igfx_off boot.shell_on_fail i915.enable_fbc=1" ];

   boot.loader.grub.devices = [
     # TODO
   ];

   # TODO
   networking.hostId = "";

   services.xserver.deviceSection = ''
     Option "DRI" "3"
     Option "TearFree" "true"
     Option "AccelMethod" "glamor"
   '';

 }
