{ config, pkgs, ... }:
# only dell latitude e6430 laptop specific settings
 {
   imports = [ ../modules ];

   boot.kernelModules = [ "microcode" "coretemp" ];
   boot.kernelParams = [ "i915.enable_fbc=1" ];
   boot.loader.grub.devices = [
     "/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76820C5544"
     "/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76822C9FD0"
   ];

   networking.hostName = "E6430";
   networking.hostId = "007f0100";

   mine.zfs.enable = true;
   mine.workstation.enable = true;
   mine.home.enable = true;
   mine.libvirtd.enable = true;
   mine.acpilight.enable = true;

   services.tlp.enable = true;

   services.xserver.libinput.enable = true;
   services.xserver.libinput.accelSpeed = "0.9";

   services.xserver.videoDrivers = [ "modesetting" ];

   # compton's glx backend activates driver vsync automatically in this particular laptop,
   # necessary because the modesetting xorg driver does not have a TearFree option.
   services.compton = {
     enable = true;
     backend = "glx";
   };
 }
