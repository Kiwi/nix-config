{ config, pkgs, lib, ... }:
# packages for all hosts
with lib;
{

imports = [];
options.modules.world.enable = mkEnableOption "World Profile";
config = mkIf config.modules.world.enable {

environment.systemPackages = with pkgs; [
wget
curl
inetutils
nix-prefetch-scripts
gptfdisk
pmutils
acpi
acpid
psmisc
which
file
binutils
bc
utillinuxCurses
exfat
dosfstools
patchutils
moreutils
unzip
zip
pciutils
lshw
usbutils
lsof
lm_sensors
htop
iotop
powertop
ltrace
strace
linuxPackages.perf
smartmontools
];

};
}
