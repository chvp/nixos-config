{ ... }:

{
  chvp.zfs.homeLinks = [
    { path = ".config/syncthing"; type = "data"; }
    { path = "sync"; type = "cache"; }
  ];
  home-manager.users.charlotte = { pkgs, ... }: {
    services.syncthing.enable = true;
  };
}
