{ config, lib, ... }:

{
  imports = [
    ./citrix
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
        citrix.enable = lib.mkDefault false;
        mounts.enable = lib.mkDefault true;
        teams.enable = lib.mkDefault true;
        vpn.enable = lib.mkDefault true;
        zotero.enable = lib.mkDefault true;
      };
    };
  };
}
