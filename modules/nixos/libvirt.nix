{ config, pkgs, ... }:
 {
   imports = [  ];

   services.dnsmasq.enable = true;
   virtualisation.libvirtd.enable = true;
   environment.variables.LIBVIRT_DEFAULT_URI = "qemu:///system";

   environment.systemPackages = with pkgs; [
     virtmanager
   ];

 }
