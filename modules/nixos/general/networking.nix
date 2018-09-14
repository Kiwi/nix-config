{ config, pkgs, ... }:
{
# firewall and ports handled by securiy.nix
networking.networkmanager.enable = true;
}
