{ ... }:

{
  custom.zfs.homeLinks = [
    { path = ".config/syncthing"; type = "data"; }
    { path = "sync"; type = "data"; }
  ];
  home-manager.users.charlotte = { pkgs, ... }: {
    services.syncthing.enable = true;
  };
}
