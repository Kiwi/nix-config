{ config, pkgs, ... }:
# only dell latitude E6430 specific settings
{
imports = [ ../../modules ];

# computer specific
networking.hostName = "E6430";

# hardware specific
boot.kernelModules = [ "microcode" "coretemp" ];
boot.kernelParams = [ "i915.enable_fbc=1" ];

# my modules
# modules.etc.enable = true; # general settings
modules.world.enable = true; # general packages
mine.workstation.enable = true; # desktop workstation packages and settings
mine.home.enable = true; # home / user related settings
mine.libvirtd.enable = true; # libvirtd specific module

# boost laptop power savings
services.tlp.enable = true;

# touchpad
services.xserver.libinput.enable = true;
services.xserver.libinput.accelSpeed = "0.9";

# xf86-video-intel works better for this laptop than modesetting :)
services.xserver.videoDrivers = [ "intel" ];
services.xserver.deviceSection = ''
Option "DRI" "3"
Option "TearFree" "true"
Option "AccelMethod" "glamor"
'';
}
