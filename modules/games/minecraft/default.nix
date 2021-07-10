{ config, lib, pkgs, ... }:

{
  options.chvp.games.minecraft = {
    client = lib.mkOption {
      default = false;
      example = true;
    };
    server = lib.mkOption {
      default = false;
      example = true;
    };
  };

  config = lib.mkIf (config.chvp.games.minecraft.client || config.chvp.games.minecraft.server) {
    home-manager.users.charlotte = lib.mkIf config.chvp.games.minecraft.client ({ ... }: {
      home.packages = [ pkgs.minecraft ];
    });
    chvp.base = {
      zfs.homeLinks = lib.optional config.chvp.games.minecraft.client { path = ".minecraft"; type = "cache"; };
      nix.unfreePackages =
        (lib.optional config.chvp.games.minecraft.client "minecraft-launcher") ++
        (lib.optional config.chvp.games.minecraft.server "minecraft-server");
    };
    services.minecraft-server = lib.mkIf config.chvp.games.minecraft.server {
      enable = true;
      dataDir = "${config.chvp.dataPrefix}/var/lib/minecraft-server";
      eula = true;
      openFirewall = true;
    };
  };
}
