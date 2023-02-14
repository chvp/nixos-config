{ config, lib, pkgs, ... }:

{
  options.chvp.work.teams.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.work.teams.enable {
    chvp.base = {
      zfs.homeLinks = [
        { path = ".config/teams-for-linux"; type = "cache"; }
      ];
    };

    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = with pkgs; [ teams-for-linux ];
    };
  };
}
