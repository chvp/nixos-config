{ config, lib, pkgs, ... }:

{
  options.chvp.deluge-client.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.deluge-client.enable {
    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = with pkgs; [ deluge ];
    };

    chvp.zfs.homeLinks = [
      { path = ".config/deluge"; type = "data"; }
    ];
  };
}
