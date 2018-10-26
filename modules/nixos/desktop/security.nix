{ config, pkgs, lib, ... }:
with lib;
{
options.modules.desktop.security.enable = mkEnableOption "modules.desktop.security";
config = mkIf config.modules.desktop.security.enable {

environment.systemPackages = with pkgs; [
(python36.withPackages(ps: with ps; [ certifi ]))
gnutls gnupg gnupg1compat pinentry keychain
];

security.sudo.wheelNeedsPassword = false;

services.openssh.permitRootLogin = "yes";

nix.allowedUsers = [ "root" "@wheel" ];
nix.trustedUsers = [ "root" "@wheel" ];

environment.interactiveShellInit = ''
export GPG_TTY=$(tty)
'';

};
}
