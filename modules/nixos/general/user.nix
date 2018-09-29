{ config, pkgs, ... }:

{
users.users.adam = {
isNormalUser = true;
createHome = true;
extraGroups = [
"wheel"
"disk"
"audio"
"video"
"systemd-journal"
];
initialPassword = "password";
};
programs.ssh.startAgent = true;
programs.gnupg.agent.enable = true;
}
