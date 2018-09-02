{ config, pkgs, ... }:
# all hpZ620 workstation specific settings
 {
   imports = [  ];

   # TODO
   boot.kernelModules = [ "coretemp" "kvm-intel" "microcode" ];
   boot.kernelParams = [ "elevator=noop intel_iommu=on iommu=pt boot.shell_on_fail" ];

   boot.loader.grub.devices = [
     # TODO
   ];

   # TODO
   networking.hostId = "";

   # TODO
   services.xserver.videoDrivers = [ "" ];

   services.xserver.deviceSection = ''
     Option "DRI" "3"
     Option "TearFree" "true"
     Option "AccelMethod" "glamor"
   '';

 }
