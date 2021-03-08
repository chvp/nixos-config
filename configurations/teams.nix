{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ teams ];
  };

  chvp = {
    nix.unfreePackages = [ "teams" ];
    zfs.homeLinks = [
      { path = ".config/Microsoft"; type = "data"; }
    ];
  };
}
