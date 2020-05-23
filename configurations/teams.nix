{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ teams ];
  };

  custom.zfs.homeLinks = [
    { path = ".config/Microsoft"; type = "data"; }
  ];
}
