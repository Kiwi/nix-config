{ config, pkgs, ... }:
let
in {

environment.systemPackages = with pkgs; [
(python36.withPackages(ps: with ps; [ certifi ]))
gnutls gnupg gnupg1compat
openvpn
];

security.sudo.wheelNeedsPassword = false;

programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

networking = {
firewall = {
allowedTCPPorts = [ 22 ];
allowedUDPPorts = [ 22 ];
allowPing = true;
};
enableIPv6 = false;
nameservers = [ "8.8.8.8" "8.8.4.4" ];
};

services.openssh.permitRootLogin = "yes";

nix.allowedUsers = [ "root" "@wheel" ];
nix.trustedUsers = [ "root" "@wheel" ];

}
