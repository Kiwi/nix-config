{ config, pkgs, ... }:
# Dell Latitude E6430 Machine-unique Hardware & Software Configuration
{
imports = [ ../../modules ];

system.stateVersion = "18.03";

# Computer name
networking.hostName = "adamant";

# use a generic latitudeE6430 profile
modules.hardware.platform.latitudeE6430.enable = true;

# Desktop
modules.desktop.enable = true;           # Generic Xorg, Mesa, Browser apps
modules.desktop.wmsupport.enable = true; # Better support for Window Managers (No Desktop Environment)
modules.desktop.exwm.enable = true;      # Use Emacs as a window manager
modules.desktop.developer.enable = true; # My developer profile
modules.desktop.security.enable = true;  # My security settings for desktops

# Services
modules.services.libvirtd.enable = true; # Virtual machines
}
