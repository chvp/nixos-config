{ config, lib, pkgs, ... }:

{
  options.chvp.zotero.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.zotero.enable {
    chvp.zfs.homeLinks = [{ path = ".zotero"; type = "data"; }];
    home-manager.users.charlotte = { ... }: {
      home.packages = [ pkgs.zotero ];
    };
  };
}
