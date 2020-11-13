{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ deluge ];
  };

  chvp.zfs.homeLinks = [
    { path = ".config/deluge"; type = "data"; }
  ];
}
