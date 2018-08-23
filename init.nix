{ ... }:
 {
   imports = [
     # Machine specific, uncomment only one:
     ./hosts/latitudeE6430.nix
     # ./hosts/hpZ620.nix

     # Machine independant:
     ./hosts/modules/etc.nix         # General configurations
     ./hosts/modules/desktop.nix     # Desktop configurations
     ./hosts/modules/home.nix        # $HOME and developer environment setup
     ./hosts/modules/rescue_boot.nix # NixOS rescue on the grub menu
   ];
 }
