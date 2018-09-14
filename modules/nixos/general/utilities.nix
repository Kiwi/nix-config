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
coreutils wget
pmutils acpi acpid lm_sensors smartmontools
gptfdisk exfat dosfstools
unzip zip
lsof htop iotop powertop tree pstree
ltrace strace linuxPackages.perf
aspell aspellDicts.en
nix-prefetch-scripts nixops nix-index
];

}
