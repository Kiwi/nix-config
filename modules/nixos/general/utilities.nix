{ config, pkgs, ... }:
{

programs = {
info.enable = true;
less.enable = true;
man.enable = true;
mtr.enable = true;
bash.enableCompletion = true;
};

environment.systemPackages = with pkgs; [
nix-prefetch-scripts nixops nix-index
coreutils pciutils
gptfdisk exfat dosfstools
unzip zip
lsof htop iotop powertop tree pstree
ltrace strace linuxPackages.perf
wget
];

}
