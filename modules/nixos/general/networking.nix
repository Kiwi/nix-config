{ config, pkgs, ... }:
{
# firewall and ports handled by security.nix
networking.networkmanager.enable = true;
networking.networkmanager.dhcp = "dhcpcd";
services.openssh.enable = true;
}
