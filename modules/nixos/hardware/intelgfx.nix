{ config, pkgs, lib, ... }:
with lib;
{
options.modules.intelgfx.enable = mkEnableOption "Intelgfx Profile";
config = mkIf config.modules.intelgfx.enable {
services.xserver.videoDrivers = [ "modesetting" ];
services.xserver.deviceSection = ''
Option "DRI" "3"
Option "TearFree" "true"
Option "AccelMethod" "glamor"
'';

services.compton = {
enable = true;
backend = "glx";
};

hardware.opengl.extraPackages = with pkgs;
[ vaapiIntel libvdpau-va-gl vaapiVdpau ];
hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux;
[ vaapiIntel libvdpau-va-gl vaapiVdpau ];

environment.sessionVariables = {
LIBVA_DRIVER_NAME = "i965";
VDPAU_DRIVER = "va_gl";
};

programs.light.enable = true;
};
}
