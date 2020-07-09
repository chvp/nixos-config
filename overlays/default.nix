{ ... }:
let
  files = [
    "firefox.nix"
    "pass.nix"
    "ssh.nix"
    "zeroad.nix"
  ];
  overlays = map (f: import (./. + "/${f}")) files;
  set = builtins.listToAttrs (map (f: { name = f; value = (./. + "/${f}"); }) files);
in
{
  nixpkgs.overlays = overlays;
}
