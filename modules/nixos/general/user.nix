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
openssh.authorizedKeys.keys =
[ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDVukS7izRS8xtTgAGcqi0UceqWV2EU/Fj9Z7cvfOwqrxMY0ffuyvvqE3Xez/CVuM+1QY/QECBUZjuurG7G2SubkHsH9j+n5b9fdSx5mzZ/jvzplSluJn/jrv88EmnMGwGv4/ylKi6FVFHhOUGWLu8cfISEe/6ZhZFWANFUSpSfXssvLVjDritazdIf8KEvZoFDw7AX+xf1YJ87WJA8ZENbsWhmI5U6nPat4rVIp4bgBcoMtukaktDdGWWxhbJLIaTJ+xHXHZ0yG+qzqg9kEF4KL1X3/sJdjKA7IvrRStK/aSiN3bXFfIA9WX5tFgJETDC4GEE9KdoMVEi3Fw9v3XbF adam@zbox" ];
};
}
