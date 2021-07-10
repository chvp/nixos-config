{ config, lib, ... }:

{
  options = {
    chvp.games.teeworlds.server = lib.mkOption {
      default = false;
      example = true;
    };
  };

  config = lib.mkIf config.chvp.games.teeworlds.server {
    services.teeworlds = {
      enable = true;
      openPorts = true;
      extraOptions = [
        "sv_gametype ctf"
        "sv_maprotation ctf1 ctf2 ctf3 ctf4 ctf5 ctf6 ctf7 ctf8"
        "sv_map ctf1"
        "sv_scorelimit 250"
        "sv_teamdamage 1"
      ];
    };
  };
}
