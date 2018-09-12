{ config, pkgs, lib, ... }:
with lib;
{
imports = [];
options.modules.general.enable = mkEnableOption "General Profile";
config = mkIf config.modules.general.enable {

i18n = {
consoleFont = "Lat2-Terminus16";
consoleKeyMap = "us";
defaultLocale = "en_US.UTF-8";
};

time.timeZone = "America/Los_Angeles";

programs.mtr.enable = true;
programs.bash.enableCompletion = true;

networking.enableIPv6 = false;
networking.networkmanager.enable = true;
networking.firewall.allowPing = true;
networking.firewall.allowedTCPPorts = [ 22 ];
networking.firewall.allowedUDPPorts = [ 22 ];
networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];

services.openssh.enable = true;
powerManagement.enable = true;

nix.allowedUsers = [ "root" "@wheel" ];
nix.trustedUsers = [ "root" "@wheel" ];
nix.useSandbox = true;

nixpkgs.config.allowUnfree = true;

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
