{ ... }: {
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ firefox ];
  };

  custom.zfs.homeLinks = [
    { path = ".cache/mozilla"; type = "cache"; }
    { path = ".mozilla"; type = "data"; }
  ];
}
