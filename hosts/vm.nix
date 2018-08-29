{ config, pkgs, ... }:
# zfs test vm settings
 {
   imports = [
     ../modules/nixos/nixos-etc.nix
     ../modules/nixos/nixos-zfs.nix
   ];

   boot.kernelModules = [ "microcode" ];
   boot.kernelParams = [ "" ];

   # put devices all on one line if using nix-on-zroot.sh (for now...)
   boot.loader.grub.devices = [ "/dev/sda" ];

   mine.zfs.enable = true;

   systemd.services.sshd.wantedBy = pkgs.lib.mkForce [ "multi-user.target" ];
   users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDVukS7izRS8xtTgAGcqi0UceqWV2EU/Fj9Z7cvfOwqrxMY0ffuyvvqE3Xez/CVuM+1QY/QECBUZjuurG7G2SubkHsH9j+n5b9fdSx5mzZ/jvzplSluJn/jrv88EmnMGwGv4/ylKi6FVFHhOUGWLu8cfISEe/6ZhZFWANFUSpSfXssvLVjDritazdIf8KEvZoFDw7AX+xf1YJ87WJA8ZENbsWhmI5U6nPat4rVIp4bgBcoMtukaktDdGWWxhbJLIaTJ+xHXHZ0yG+qzqg9kEF4KL1X3/sJdjKA7IvrRStK/aSiN3bXFfIA9WX5tFgJETDC4GEE9KdoMVEi3Fw9v3XbF adam@zbox"
  ];

   networking = {
     hostName = "nixvm";
     usePredictableInterfaceNames = false;
     interfaces.eth0.ip4 = [{
      address = "192.168.122.99";
      prefixLength = 24;
    }];
    defaultGateway = "192.168.122.1";
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    firewall.allowPing = true;
    firewall.allowedTCPPorts = [ 22 ];
    firewall.allowedUDPPorts = [ 22 ];
  };

   # Bootstrap Block - Set one variable per line beginning with #|
     #|POOL_NAME=zroot
     #|POOL_TYPE=raidz1
     #|POOL_DISKS=/dev/sda /dev/sdb /dev/sdc
     #|POOL_HOSTID=random
     #|NIXCFG_LOCATION=/nix-config/
     #|REMOVE_REMNANTS=true
     #|ATIME=false
     #|SNAPSHOT_ROOT=true
     #|SNAPSHOT_HOME=true
     #|USE_ZSWAP=true
     #|ZSWAP_SIZE=4G
 }
