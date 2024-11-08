{ config, lib, pkgs, ... }:
{
  nix.gc.dates = if config.chvp.base.nix.slowGc then "daily" else "hourly";
  nix.optimise.dates = [ "hourly" ];
  programs.command-not-found.enable = false;
}
