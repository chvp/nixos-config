{ ... }:
let
  files = [
    "pass.nix"
    "ssh.nix"
    "zeroad.nix"
  ];
  overlays = map (f: import (./. + "/${f}")) files;
  set = builtins.listToAttrs (map (f: { name = f; value = (./. + "/${f}"); }) files);
in
{
  nixpkgs.overlays = overlays;

  home-manager.users.charlotte = { pkgs, lib, ... }: {
    xdg.configFile = lib.attrsets.mapAttrs' (name: value: { name = "nixpkgs/overlays/${name}"; value = { source = value; }; }) set;
    nixpkgs.overlays = overlays;
  };
}
