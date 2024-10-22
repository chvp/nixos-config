{ config, lib, pkgs, ... }:

{
  options.chvp.graphical.theme.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.theme.enable {
    chvp.base.zfs.homeLinks = [
      { path = ".config/qt5ct"; type = "cache"; }
      { path = ".config/qt6ct"; type = "cache"; }
    ];
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
      packages = with pkgs; [
        hack-font
        font-awesome
        noto-fonts
        noto-fonts-cjk-sans
        noto-fonts-emoji
        noto-fonts-extra
        roboto
      ];
    };

    programs.dconf.enable = true;
    home-manager.users.charlotte = { pkgs, lib, ... }: {
      home.packages = [
        pkgs.catppuccin-cursors.latteLight
        # Also install dark mode to profile for darkman
        (pkgs.catppuccin-gtk.override { size = "compact"; variant = "frappe"; })
      ];
      home.file = {
        ".icons/default/index.theme".text = ''
          [Icon Theme]
          Name=Default
          Comment=Default Cursor Theme
          Inherits=catppuccin-latte-light-cursors
        '';
      };
      dconf.settings."org/gnome/desktop/wm/preferences".button-layout = "";
      gtk = {
        enable = true;
        font = {
          package = pkgs.noto-fonts;
          name = "Noto Sans";
          size = 10;
        };
        gtk2.extraConfig = ''
          gtk-cursor-theme-name = "catppuccin-latte-light-cursors"
          gtk-cursor-theme-size = 24
        '';
        gtk3 = {
          extraConfig = {
            gtk-cursor-theme-name = "catppuccin-latte-light-cursors";
            gtk-cursor-theme-size = 24;
          };
          extraCss = ''
            /* No (default) titlebar on wayland */
            headerbar.titlebar.default-decoration {
              background: transparent;
              padding: 0;
              margin: 0 0 -17px 0;
              border: 0;
              min-height: 0;
              font-size: 0;
              box-shadow: none;
            }

            /* rm -rf window shadows */
            window.csd,             /* gtk4? */
            window.csd decoration { /* gtk3 */
              box-shadow: none;
            }
          '';
        };
        gtk4 = {
          extraConfig = {
            gtk-cursor-theme-name = "catppuccin-latte-light-cursors";
            gtk-cursor-theme-size = 24;
          };
          extraCss = ''
            /* No (default) titlebar on wayland */
            headerbar.titlebar.default-decoration {
              background: transparent;
              padding: 0;
              margin: 0 0 -17px 0;
              border: 0;
              min-height: 0;
              font-size: 0;
              box-shadow: none;
            }

            /* rm -rf window shadows */
            window.csd,             /* gtk4? */
            window.csd decoration { /* gtk3 */
              box-shadow: none;
            }
          '';
        };
        iconTheme = {
          package = pkgs.libsForQt5.breeze-icons;
          name = "breeze";
        };
        theme = {
          package = pkgs.catppuccin-gtk.override { size = "compact"; variant = "latte"; };
          name = "Catppuccin-Latte-Compact-Blue-Light";
        };
      };
      qt = {
        enable = true;
        platformTheme.name = "qtct";
        style = {
          name = "lightly";
          package = pkgs.lightly-qt;
        };
      };
      services.darkman = {
        enable = true;
        settings = {
          lat = 51.0;
          lng = 3.7;
          usegeoclue = false;
          dbusserver = true;
          portal = true;
        };
        darkModeScripts = {
          emacs = ''
            emacsclient --eval "(chvp--dark-mode)"
          '';
          gtk = ''
            ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme Catppuccin-Frappe-Compact-Blue-Dark
          '';
          river = ''
            riverctl background-color 0x626880
            riverctl border-color-focused 0x99d1db
            riverctl border-color-unfocused 0x232634
            riverctl border-color-urgent 0xf4b8e4
          '';
          qt = ''
            sed -i "s/Latte/Frappe/" ~/.config/qt5ct/qt5ct.conf
            sed -i "s/Latte/Frappe/" ~/.config/qt6ct/qt6ct.conf
          '';
          terminal = ''
            pkill -SIGUSR2 zsh
          '';
          waybar = ''
            ln -sf ~/.config/waybar/frappe.css ~/.config/waybar/colors.css
            systemctl --user restart waybar.service
          '';
        };
        lightModeScripts = {
          emacs = ''
            emacsclient --eval "(chvp--light-mode)"
          '';
          gtk = ''
            ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme Catppuccin-Latte-Compact-Blue-Light
          '';
          river = ''
            riverctl background-color 0xacb0be
            riverctl border-color-focused 0x04e5e5
            riverctl border-color-unfocused 0xdce0e8
            riverctl border-color-urgent 0xea76cb
          '';
          qt = ''
            sed -i "s/Frappe/Latte/" ~/.config/qt5ct/qt5ct.conf
            sed -i "s/Frappe/Latte/" ~/.config/qt6ct/qt6ct.conf
          '';
          terminal = ''
            pkill -SIGUSR1 zsh
          '';
          waybar = ''
            ln -sf ~/.config/waybar/latte.css ~/.config/waybar/colors.css
            systemctl --user restart waybar.service
          '';
        };
      };
      home.activation = {
        linkWaybarCssColors = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD ln -sf $VERBOSE_ARG ~/.config/waybar/latte.css ~/.config/waybar/colors.css
        '';
      };
    };
  };
}
