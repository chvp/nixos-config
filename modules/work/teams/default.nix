{ config, lib, pkgs, ... }:

{
  options.chvp.work.teams.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.work.teams.enable {
    chvp.base = {
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
