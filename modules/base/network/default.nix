{ ... }:

{
  imports = [
    ./ovh.nix
    ./mobile.nix
    ./tailscale.nix
  ];

  networking.firewall.checkReversePath = "loose";
}
