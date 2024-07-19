{ config, ... }:

{
  nix = {
    configureBuildUsers = true;
    gc.interval = if config.chvp.base.nix.slowGc then { Hour = 0; Minute = 0; } else { Minute = 0; };
    settings.sandbox = true;
  };
  services.nix-daemon.enable = true;
}
