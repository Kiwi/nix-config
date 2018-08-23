{ lib, ... }: {
  imports = import /nix-config/lib/recimport.nix { inherit lib; dir = ./.; };
}
