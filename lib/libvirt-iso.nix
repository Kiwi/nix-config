# nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=libvirt-iso.nix
{config, pkgs, ...}:
let
  themelios = pkgs.writeScriptBin "themelios" ''
    bash <(curl https://raw.githubusercontent.com/a-schaefers/themelios/master/themelios) $@
  '';
in {
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];
  networking = {
    usePredictableInterfaceNames = false;
    interfaces.eth0.ip4 = [{
      address = "192.168.122.99";
      prefixLength = 24;
    }];
    defaultGateway = "192.168.122.1";
    firewall.allowPing = true;
    firewall.allowedTCPPorts = [ 22 ];
    firewall.allowedUDPPorts = [ 22 ];
  };
  boot.supportedFilesystems = [ "zfs" ];
  environment.systemPackages = with pkgs; [ themelios git ];
}
