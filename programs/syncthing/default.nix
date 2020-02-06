{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    services.syncthing.enable = true;
  };
}
