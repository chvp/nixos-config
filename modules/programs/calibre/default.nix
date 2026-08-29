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
  options.chvp.programs.calibre.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.programs.calibre.enable {
    chvp.base.zfs.homeLinks = [
      {
        path = ".config/calibre";
        type = "cache";
      }
    ];
    home-manager.users.${username}.home.packages = [ pkgs.calibre ];
    services.udisks2.enable = true;
  };
}
