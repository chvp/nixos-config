{ config, lib, pkgs, ... }:

{
  options.chvp.work.zotero.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.work.zotero.enable {
    chvp.base.zfs.homeLinks = [{ path = ".zotero"; type = "data"; }];
    home-manager.users.charlotte = { ... }: {
      home.packages = [ pkgs.zotero ];
    };
  };
}
