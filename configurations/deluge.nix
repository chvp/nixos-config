{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ deluge ];
  };

  custom.zfs.homeLinks = [
    { path = ".config/deluge"; type = "data"; }
  ];
}
