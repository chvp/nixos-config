{ ... }:
let
  overlays = [
    (import ./pass.nix)
    (import ./ssh.nix)
  ];
in
{
  nixpkgs.overlays = overlays;

  home-manager.users.charlotte = { pkgs, ... }: {
    nixpkgs.overlays = overlays;
  };
}
