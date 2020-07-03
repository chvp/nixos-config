{ ... }:

{
  custom.zfs.homeLinks = [
    { path = ".config/0ad"; type = "cache"; }
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [
      pkgs.zeroad
    ];
  };
}
