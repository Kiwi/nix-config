{ config, pkgs, ... }:
# all dell latitude e6430 laptop specific settings
 {
   imports = [
     ./modules/libvirt.nix
   ];

   boot.kernelParams = [ "elevator=noop iommu=pt intel_iommu=igfx_off boot.shell_on_fail i915.enable_fbc=1" ];

   # grub / hardware (using id's instead of /dev/sda /dev/sdb is better)
   boot.loader.grub.devices = [
     "/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76820C5544"
     "/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76822C9FD0"
   ];

   # important!! for the specific ZFS pool of this computer!
   networking.hostId = "007f0100";

   # use acpilight xbacklight drop-in replacement
   environment.systemPackages = with pkgs; [
     pkgs.acpilight
   ];
   nixpkgs.config.packageOverrides = super: {
     acpilight = pkgs.callPackage ./modules/acpilight.nix {};
   };

   # extra power savings
   services.tlp.enable = true;

   # touchpad
   services.xserver.libinput.enable = true;
   services.xserver.libinput.accelSpeed = "0.9";

   # graphics
   services.xserver.videoDrivers = [ "modesetting" ];
      services.xserver.deviceSection = ''
        Option "DRI" "3"
        Option "AccelMethod" "glamor"
   '';

   # glx activates driver vsync automatically in this particular laptop,
   # necessary because the modesetting xorg driver does not have a TearFree option
   services.compton = {
     enable = true;
     backend = "glx";
     # vsync = "opengl";
   };

 }
