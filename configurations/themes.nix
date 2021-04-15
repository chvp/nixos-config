{ ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ pkgs.vanilla-dmz ];
    home.file = {
      ".icons/default/index.theme".text = ''
        [Icon Theme]
        Name=Default
        Comment=Default Cursor Theme
        Inherits=Vanilla-DMZ
      '';
    };
    dconf.settings."org/gnome/desktop/interface" = {
      gtk-theme = "Arc";
      icon-theme = "Arc";
      cursor-theme = "Vanilla-DMZ";
    };
    gtk = {
      enable = true;
      font = {
        package = pkgs.noto-fonts;
        name = "Noto Sans";
        size = 10;
      };
      gtk2.extraConfig = ''
        gtk-cursor-theme-name = "Vanilla-DMZ"
        gtk-cursor-theme-size = 0
      '';
      gtk3.extraConfig = {
        gtk-cursor-theme-name = "Vanilla-DMZ";
        gtk-cursor-theme-size = 0;
      };
      iconTheme = {
        package = pkgs.arc-icon-theme;
        name = "Arc";
      };
      theme = {
        package = pkgs.arc-theme;
        name = "Arc";
      };
    };
    qt = {
      enable = true;
      platformTheme = "gtk";
    };
  };
}
