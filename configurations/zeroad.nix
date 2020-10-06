{ ... }:

{
  custom.zfs.homeLinks = [
    { path = ".config/0ad"; type = "cache"; }
  ];

  nixpkgs.config.permittedInsecurePackages = [
    "spidermonkey-38.8.0"
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ pkgs.zeroad ];
  };
}
