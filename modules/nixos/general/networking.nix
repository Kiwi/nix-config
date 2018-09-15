{ config, pkgs, ... }:
{
services.openssh.enable = true;

networking = {
networkmanager = {
enable = true;
dhcp = "dhcpcd";
};
firewall = {
allowedTCPPorts = [ 22 ];
allowedUDPPorts = [ 22 ];
allowPing = true;
};
nameservers = [ "8.8.8.8" "8.8.4.4" ];
};
}
