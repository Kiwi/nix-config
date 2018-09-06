{ config, pkgs, ... }:
# only dell latitude e6430 laptop specific settings
 {
   imports = [ ../../modules ];

   # boot.kernelModules = [ "microcode" "coretemp" ];
   # boot.kernelParams = [ "i915.enable_fbc=1" ];
   networking.hostName = "testrun";

   mine.workstation.enable = true;
   mine.home.enable = true;
   # mine.libvirtd.enable = true;
   # mine.acpilight.enable = true;

   # services.tlp.enable = true;

   # services.xserver.libinput.enable = true;
   # services.xserver.libinput.accelSpeed = "0.9";

   # services.xserver.videoDrivers = [ "modesetting" ];

   # compton's glx backend activates driver vsync automatically in this particular laptop,
   # necessary because the modesetting xorg driver does not have a TearFree option.
 #   services.compton = {
   #     enable = true;
   #     backend = "glx";
   #   };
 }
