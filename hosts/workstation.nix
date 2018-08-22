{ config, pkgs, ... }:
 {
   imports = [  ];
   boot.kernelParams = [ "elevator=noop intel_iommu=on iommu=pt boot.shell_on_fail i915.enable_fbc=1" ];
 }
