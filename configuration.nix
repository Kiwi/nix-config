{ config, pkgs, ... }:

{
imports = [
./hardware-configuration.nix
];

boot.loader.grub.enable = true;
boot.loader.grub.version = 2;
boot.loader.grub.device = "/dev/sda";
boot.supportedFilesystems = [ "zfs" ];
boot.kernelParams = [ "elevator=noop intel_iommu=on iommu=pt" ];
networking.hostId = "007f0100";
networking.hostName = "nixos";
networking.networkmanager.enable = true;

i18n = {
consoleFont = "Lat2-Terminus16";
consoleKeyMap = "us";
defaultLocale = "en_US.UTF-8";
};

time.timeZone = "America/Los_Angeles";

environment.sessionVariables = {
EDITOR = "emacsclient";
VISUAL = "emacsclient";
};

environment.shells = [
"${pkgs.zsh}/bin/bash"
];

environment.systemPackages = with pkgs; [
wget
curl
gnutls
gnupg
gnupg1compat
pinentry
git
firefox
tmux
mpv
wmctrl
# xorg.xbacklight
xorg.xmodmap
xorg.xset
xorg.xsetroot
libnotify
pkgs.acpilight
];

nixpkgs.config.packageOverrides = super: {
    acpilight = pkgs.callPackage ./pkgs/acpilight.nix {};
  };

programs.bash.enableCompletion = true;
programs.mtr.enable = true;
programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

services.openssh.enable = true;

networking.firewall.allowedTCPPorts = [ 22 ];
networking.firewall.allowedUDPPorts = [ 22 ];

sound.enable = true;
hardware.pulseaudio.enable = true;

hardware.opengl = {
driSupport = true;
driSupport32Bit = true;
};

services.xserver = {
enable = true;
layout = "us";
libinput.enable = true;
videoDrivers = [ "modesetting" ];
useGlamor = true;
deviceSection = ''
Option "DRI" "3"
Option "TearFree" "true"
Option "AccelMethod" "glamor"
'';
displayManager.slim.enable = true;
displayManager.slim.defaultUser = "adam";
desktopManager = {
default = "emacs";
session = [ {
manage = "desktop";
name = "emacs";
start = ''
${pkgs.xlibs.xsetroot}/bin/xsetroot -cursor_name left_ptr
${pkgs.xlibs.xset}/bin/xset r rate 250 50
${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap
${pkgs.emacs}/bin/emacs &
waitPID=$!
'';
} ];
};
};

fonts.fonts = with pkgs; [
liberation_ttf
noto-fonts
noto-fonts-cjk
noto-fonts-emoji
source-code-pro
font-awesome-ttf
powerline-fonts
];

users.users.adam =
{ isNormalUser = true;
home = "/home/adam";
createHome = false;
extraGroups = [ "wheel" "disk" "audio" "video" "networkmanager" "systemd-journal" ];
};
security.sudo.wheelNeedsPassword = false;

powerManagement.enable = true;

# This value determines the NixOS release with which your system is to be
# compatible, in order to avoid breaking some software such as database
# servers. You should change this only after NixOS release notes say you
# should.
system.stateVersion = "18.03"; # Did you read the comment?

}
