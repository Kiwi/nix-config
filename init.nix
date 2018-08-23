{ ... }:
 {
   imports = [
     # Machine specific, uncomment only one:
     ./hosts/latitudeE6430.nix
     # ./hosts/hpZ620.nix

     # Machine independant:
     ./modules/rescue_boot.nix # NixOS rescue on the grub menu
     ./modules/etc.nix         # General configurations
     ./modules/desktop.nix     # Desktop configurations
     ./modules/home.nix        # $HOME and developer environment setup
   ];
 }
