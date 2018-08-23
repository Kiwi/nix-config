{ ... }:
 {
   imports = [
     ./hosts/latitudeE6430.nix       # machine specific settings
     # ./hosts/hpZ620.nix            # machine specific settings
     ./hosts/modules/etc.nix         # system-wide general settings
     ./hosts/modules/desktop.nix     # system-wide desktop settings
     ./hosts/modules/home.nix        # home settings
     ./hosts/modules/rescue_boot.nix # NixOS rescue on the grub menu
   ];
 }
