{ config, pkgs, lib, ... }:
with lib;
# TODO udev rules to work without sudo
  {
    imports = [  ];

    options.mine.acpilight.enable = mkEnableOption "Acpilight xbacklight drop-in replacement";
    config = mkIf config.mine.acpilight.enable {

      environment.systemPackages = with pkgs; [
        pkgs.acpilight
      ];
      nixpkgs.config.packageOverrides = super: {
        acpilight = pkgs.callPackage ../../../pkgs/acpilight.nix {};
      };

    };

  }
