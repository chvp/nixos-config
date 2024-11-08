{ config, ... }:

{
  nix = {
    configureBuildUsers = true;
    gc.interval = if config.chvp.base.nix.slowGc then { Hour = 0; Minute = 0; } else { Minute = 0; };
    optimise.interval = { Minute = 30; };
    settings.sandbox = true;
  };
  nixpkgs.flake = {
    setFlakeRegistry = false;
    setNixPath = false;
  };
  services.nix-daemon.enable = true;
}
