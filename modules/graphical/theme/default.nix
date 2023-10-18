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
        noto-fonts-cjk
        noto-fonts-emoji
        noto-fonts-extra
      ];
    };

    programs.dconf.enable = true;
    home-manager.users.charlotte = { pkgs, ... }: {
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
          Inherits=Catppuccin-Latte-Light-Cursors
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
          gtk-cursor-theme-name = "Catppuccin-Latte-Light-Cursors"
          gtk-cursor-theme-size = 24
        '';
        gtk3 = {
          extraConfig = {
            gtk-cursor-theme-name = "Catppuccin-Latte-Light-Cursors";
            gtk-cursor-theme-size = 24;
          };
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
        platformTheme = "qtct";
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
            emacsclient --eval "(setq catppuccin-flavor 'frappe)"
            emacsclient --eval "(load-theme 'catppuccin :no-confirm)"
          '';
          gtk = ''
            ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme Catppuccin-Frappe-Compact-Blue-Dark
          '';
          qt = ''
            sed -i "s/Latte/Frappe/" ~/.config/qt5ct/qt5ct.conf
            sed -i "s/Latte/Frappe/" ~/.config/qt6ct/qt6ct.conf
          '';
        };
        lightModeScripts = {
          emacs = ''
            emacsclient --eval "(setq catppuccin-flavor 'latte)"
            emacsclient --eval "(load-theme 'catppuccin :no-confirm)"
          '';
          gtk = ''
            ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme Catppuccin-Latte-Compact-Blue-Light
          '';
          qt = ''
            sed -i "s/Frappe/Latte/" ~/.config/qt5ct/qt5ct.conf
            sed -i "s/Frappe/Latte/" ~/.config/qt6ct/qt6ct.conf
          '';
        };
      };
    };
  };
}
