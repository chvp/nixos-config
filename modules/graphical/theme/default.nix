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
          # The Tinos and Amiro fonts overlap with Font Awesome's codepoints, so make sure we give Font Awesome a higher priority.
          monospace = [ "Hack" "Font Awesome 6 Free" ];
          sansSerif = [ "Noto Sans" "Font Awesome 6 Free" ];
          serif = [ "Noto Serif" "Font Awesome 6 Free" ];
        };
      };
      fonts = with pkgs; [
        hack-font
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
      dconf.settings = {
        "org/gnome/desktop/interface" = {
          gtk-theme = "Breeze";
          icon-theme = "breeze";
          cursor-theme = "Vanilla-DMZ";
        };
        "org/gnome/desktop/wm/preferences".button-layout = "";
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
          gtk-cursor-theme-size = 24
        '';
        gtk3 = {
          extraConfig = {
            gtk-cursor-theme-name = "Vanilla-DMZ";
            gtk-cursor-theme-size = 24;
          };
          extraCss = ''
            headerbar {
                min-height: 0px;
                padding: 0px;
                margin: 0px;
            }

            headerbar entry,
            headerbar spinbutton,
            headerbar button,
            headerbar separator {
                min-height: 0px;
                padding: 0px;
                margin: 0px;
            }
          '';
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
