{ config, pkgs, ... }:
{
boot.kernelModules = [ "microcode" ];
}
