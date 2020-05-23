{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ joplin-desktop ];
  };

  custom.zfs.homeLinks = [
    { path = ".config/joplin-desktop"; type = "data"; }
    { path = ".config/Joplin"; type = "data"; }
  ];
}
