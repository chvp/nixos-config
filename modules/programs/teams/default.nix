{ config, lib, pkgs, ... }:

let
  teamsWrapper = pkgs.writeShellScriptBin "teams-for-linux" ''
    ${pkgs.teams-for-linux}/bin/teams-for-linux --followSystemTheme
  '';
in
{
  options.chvp.programs.teams.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.programs.teams.enable {
    chvp.base = {
      zfs.homeLinks = [
        { path = ".config/teams-for-linux"; type = "cache"; }
      ];
    };

    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = with pkgs; [ teamsWrapper ];
    };
  };
}
