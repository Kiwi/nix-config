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
"networkmanager"
"libvirtd"
];
initialPassword = "password";
};
}
