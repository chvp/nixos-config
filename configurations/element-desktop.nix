{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ element-desktop ];
  };

  custom.zfs.homeLinks = [
    { path = ".config/Element"; type = "data"; }
  ];
}
