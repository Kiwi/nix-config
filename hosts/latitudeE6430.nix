{ config, pkgs, ... }:
# all dell latitude e6430 laptop specific settings
 {
   imports = [
     ../modules
   ];

   networking.hostName = "E6430";

   mine.zfsCare.enable = true;
   mine.home.enable = true;
   mine.workstation.enable = true;
   mine.libvirtd.enable = true;
   mine.acpilight.enable = true;

   boot.kernelModules = [ "coretemp" "kvm-intel" "microcode" ];
   boot.kernelParams = [ "elevator=noop boot.shell_on_fail i915.enable_fbc=1" ];

   boot.loader.grub.devices = [
     "/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76820C5544"
     "/dev/disk/by-id/ata-KINGSTON_SA400S37120G_50026B76822C9FD0"
   ];

   # important!! for the specific ZFS pool of this computer!
   networking.hostId = "007f0100";

   services.tlp.enable = true;

   services.xserver.libinput.enable = true;
   services.xserver.libinput.accelSpeed = "0.9";

   services.xserver.videoDrivers = [ "modesetting" ];
      services.xserver.deviceSection = ''
        Option "DRI" "3"
        Option "AccelMethod" "glamor"
   '';

   # compton's glx backend activates driver vsync automatically in this particular laptop,
   # necessary because the modesetting xorg driver does not have a TearFree option.
   services.compton = {
     enable = true;
     backend = "glx";
     # vsync = "opengl";
   };

 }
