{
  config,
  lib,
  pkgs,
  ...
}:

let
  username = config.chvp.username;
in
{
  options.chvp.games.mumble.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.games.mumble.enable {
    chvp.base.zfs.homeLinks = [
      {
        path = ".config/Mumble";
        type = "cache";
      }
      {
        path = ".local/share/Mumble";
        type = "cache";
      }
    ];

    home-manager.users.${username} = {
      home.packages = with pkgs; [ mumble ];
    };
  };
}
