{ config, pkgs, ... }:
 {
   imports = [  ];

   boot.kernelModules = [ "coretemp" "kvm-intel" "microcode" ];
   boot.kernelParams = [ "elevator=noop iommu=pt intel_iommu=igfx_off boot.shell_on_fail i915.enable_fbc=1" ];

   boot.loader.grub.devices = [
     "/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76820C5544"
     "/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76822C9FD0"
   ];

   networking.hostId = "007f0100";

   environment.systemPackages = with pkgs; [
     pkgs.acpilight
   ];

   nixpkgs.config.packageOverrides = super: {
     acpilight = pkgs.callPackage ../modules/acpilight.nix {};
   };

   services.tlp.enable = true;

   services.xserver.libinput.enable = true;
   services.xserver.libinput.accelSpeed = "0.9";
   services.xserver.videoDrivers = [ "modesetting" ];
   services.xserver.useGlamor = true;
   services.xserver.deviceSection = ''
     Option "DRI" "3"
     Option "TearFree" "true"
     Option "AccelMethod" "glamor"
   '';

   services.compton = {
     enable = true;
     backend = "glx";
   };

 }
