{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.theme.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.theme.enable {
    fonts = {
      fontDir.enable = true;
      fontconfig = {
        enable = true;
        defaultFonts = {
          emoji = [ "Noto Color Emoji" ];
          monospace = [ "Fira Code" ];
          sansSerif = [ "Noto Sans" ];
          serif = [ "Noto Serif" ];
        };
      };
      fonts = with pkgs; [
        fira-code
        fira-code-symbols
        font-awesome
        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji
        noto-fonts-extra
      ];
    };

    programs.dconf.enable = true;
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
        gtk-theme = "Breeze";
        icon-theme = "breeze";
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
          package = pkgs.libsForQt5.breeze-icons;
          name = "breeze";
        };
        theme = {
          package = pkgs.libsForQt5.breeze-gtk;
          name = "Breeze";
        };
      };
      qt = {
        enable = true;
        platformTheme = "gnome";
        style = {
          name = "breeze";
          package = pkgs.libsForQt5.breeze-qt5;
        };
      };
    };
  };
}
