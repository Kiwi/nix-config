 { ... }:
# this can be a symlink in /etc/nixos/ or the actual file.
{ imports = [
  /etc/nixos/hardware-configuration.nix
  /nixcfg/init.nix
 ]; }
