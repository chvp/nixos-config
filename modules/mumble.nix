{ config, lib, pkgs, ... }:

{
  options.chvp.mumble.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.mumble.enable {
    chvp.zfs.homeLinks = [
      { path = ".config/Mumble"; type = "data"; }
      { path = ".local/share/Mumble"; type = "data"; }
    ];

    home-manager.users.charlotte = { ... }: {
      home.packages = with pkgs; [ mumble ];
    };
  };
}
