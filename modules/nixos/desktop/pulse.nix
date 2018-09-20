{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.pulse.enable = mkEnableOption "modules.desktop.pulse";
config = mkIf config.modules.desktop.pulse.enable {

# pulse audio
hardware.pulseaudio.enable = true;

environment.systemPackages = with pkgs; [ pavucontrol ];

};
}
