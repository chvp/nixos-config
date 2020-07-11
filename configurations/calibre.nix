{ ... }:

{
  custom.zfs.homeLinks = [
    { path = ".config/calibre"; type = "cache"; }
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ pkgs.calibre ];
  };
}
