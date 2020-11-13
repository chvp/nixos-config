{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ element-desktop ];
  };

  chvp.zfs.homeLinks = [
    { path = ".config/Element"; type = "data"; }
  ];
}
