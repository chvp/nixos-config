{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ thunderbird ];
  };

  custom.zfs.homeLinks = [
    { path = ".cache/thunderbird"; type = "cache"; }
    { path = ".thunderbird"; type = "data"; }
  ];
}
