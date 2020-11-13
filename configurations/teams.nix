{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ teams ];
  };

  chvp.zfs.homeLinks = [
    { path = ".config/Microsoft"; type = "data"; }
  ];
}
