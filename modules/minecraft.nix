{ config, lib, pkgs, ... }:

{
  options.chvp.minecraft = {
    client = lib.mkOption {
      default = false;
      example = true;
    };
    server = lib.mkOption {
      default = false;
      example = true;
    };
  };

  config = lib.mkIf (config.chvp.minecraft.client || config.chvp.minecraft.server) {
    home-manager.users.charlotte = lib.mkIf config.chvp.minecraft.client ({ ... }: {
      home.packages = [ pkgs.minecraft ];
    });
    chvp.zfs.homeLinks = lib.optional config.chvp.minecraft.client { path = ".minecraft"; type = "cache"; };
    services.minecraft-server = lib.mkIf config.chvp.minecraft.server {
      enable = true;
      dataDir = "${config.chvp.dataPrefix}/var/lib/minecraft-server";
      eula = true;
      openFirewall = true;
    };
    chvp.nix.unfreePackages =
      (lib.optional config.chvp.minecraft.client "minecraft-launcher") ++
      (lib.optional config.chvp.minecraft.server "minecraft-server");
  };
}
