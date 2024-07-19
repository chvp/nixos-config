{ config, lib, pkgs, ... }:
{
  nix.gc.dates = if config.chvp.base.nix.slowGc then "daily" else "hourly";
  programs.command-not-found.enable = false;
}
