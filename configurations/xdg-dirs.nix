{ ... }:

{
  chvp.zfs.homeLinks = [
    { path = "desktop"; type = "data"; }
    { path = "documents"; type = "data"; }
    { path = "downloads"; type = "data"; }
    { path = "music"; type = "data"; }
    { path = "pictures"; type = "data"; }
    { path = "repos"; type = "data"; }
    { path = "templates"; type = "data"; }
    { path = "videos"; type = "data"; }
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ xdg-user-dirs ];
    xdg = {
      enable = true;
      userDirs = {
        enable = true;
        desktop = "\$HOME/desktop";
        documents = "\$HOME/documents";
        download = "\$HOME/downloads";
        music = "\$HOME/music";
        pictures = "\$HOME/pictures";
        publicShare = "\$HOME/desktop";
        templates = "\$HOME/templates";
        videos = "\$HOME/videos";
      };
    };
  };
}
