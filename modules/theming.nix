{ config, lib, pkgs, ... }:

{
  options.chvp.theming.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.theming.enable {
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
        font-awesome_4
        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji
        noto-fonts-extra
      ];
    };

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
  };
}
