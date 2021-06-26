{ config, lib, pkgs, ... }:

{
  options.chvp.ugent.teams.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.ugent.teams.enable {
    chvp = {
      nix.unfreePackages = [ "teams" ];
      zfs.homeLinks = [
        { path = ".config/Microsoft"; type = "data"; }
      ];
    };

    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = with pkgs; [ teams ];
    };
  };
}
