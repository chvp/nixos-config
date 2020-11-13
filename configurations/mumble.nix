{ ... }: {
  chvp.zfs.homeLinks = [
    { path = ".config/Mumble"; type = "data"; }
    { path = ".local/share/Mumble"; type = "data"; }
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ mumble ];
  };
}
