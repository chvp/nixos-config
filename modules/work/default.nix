{ config, lib, ... }:

{
  imports = [
    ./mounts
    ./teams
    ./vpn
    ./zotero
  ];

  options.chvp.work.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.work.enable {
    chvp = {
      development.enable = true;
      work = {
        mounts.enable = lib.mkDefault true;
        teams.enable = lib.mkDefault true;
        vpn.enable = lib.mkDefault true;
        zotero.enable = lib.mkDefault true;
      };
    };
  };
}
