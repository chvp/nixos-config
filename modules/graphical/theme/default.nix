{ config, lib, pkgs, ... }:

let
  catppuccin-light-colors-css = ''
    @define-color catppuccin_rosewater #dc8a78;
    @define-color catppuccin_flamingo #dd7878;
    @define-color catppuccin_pink #ea76cb;
    @define-color catppuccin_mauve #8839ef;
    @define-color catppuccin_red #d20f39;
    @define-color catppuccin_maroon #e64553;
    @define-color catppuccin_peach #fe640b;
    @define-color catppuccin_yellow #df8e1d;
    @define-color catppuccin_green #40a02b;
    @define-color catppuccin_teal #179299;
    @define-color catppuccin_sky #04a5e5;
    @define-color catppuccin_sapphire #209fb5;
    @define-color catppuccin_blue #1e66f5;
    @define-color catppuccin_lavender #7287fd;
    @define-color catppuccin_text #4c4f69;
    @define-color catppuccin_subtext1 #5c5f77;
    @define-color catppuccin_subtext0 #6c6f85;
    @define-color catppuccin_overlay2 #7c7f93;
    @define-color catppuccin_overlay1 #8c8fa1;
    @define-color catppuccin_overlay0 #9ca0b0;
    @define-color catppuccin_surface2 #acb0be;
    @define-color catppuccin_surface1 #bcc0cc;
    @define-color catppuccin_surface0 #ccd0da;
    @define-color catppuccin_base #eff1f5;
    @define-color catppuccin_mantle #e6e9ef;
    @define-color catppuccin_crust #dce0e8;
  '';
  catppuccin-dark-colors-css = ''
    @define-color catppuccin_rosewater #f2d5cf;
    @define-color catppuccin_flamingo #eebebe;
    @define-color catppuccin_pink #f4b8e4;
    @define-color catppuccin_mauve #ca9ee6;
    @define-color catppuccin_red #e78284;
    @define-color catppuccin_maroon #ea999c;
    @define-color catppuccin_peach #ef9f76;
    @define-color catppuccin_yellow #e5c890;
    @define-color catppuccin_green #a6d189;
    @define-color catppuccin_teal #81c8be;
    @define-color catppuccin_sky #99d1db;
    @define-color catppuccin_sapphire #85c1dc;
    @define-color catppuccin_blue #8caaee;
    @define-color catppuccin_lavender #babbf1;
    @define-color catppuccin_text #c6d0f5;
    @define-color catppuccin_subtext1 #b5bfe2;
    @define-color catppuccin_subtext0 #a5adce;
    @define-color catppuccin_overlay2 #949cbb;
    @define-color catppuccin_overlay1 #838ba7;
    @define-color catppuccin_overlay0 #737994;
    @define-color catppuccin_surface2 #626880;
    @define-color catppuccin_surface1 #51576d;
    @define-color catppuccin_surface0 #414559;
    @define-color catppuccin_base #303446;
    @define-color catppuccin_mantle #292c3c;
    @define-color catppuccin_crust #232634;
  '';
  gtkTheme = (pkgs.colloid-gtk-theme.override { themeVariants = [ "orange" ]; colorVariants = [ "light" "dark" ]; sizeVariants = [ "compact" ]; tweaks = [ "catppuccin" ]; }).overrideAttrs (old: {
    postInstall = ''
      echo '${catppuccin-light-colors-css}' >> $out/share/themes/Colloid-Orange-Light-Compact-Catppuccin/gtk-3.0/gtk.css
      echo '${catppuccin-light-colors-css}' >> $out/share/themes/Colloid-Orange-Light-Compact-Catppuccin/gtk-4.0/gtk.css
      echo '${catppuccin-dark-colors-css}' >> $out/share/themes/Colloid-Orange-Dark-Compact-Catppuccin/gtk-3.0/gtk.css
      echo '${catppuccin-dark-colors-css}' >> $out/share/themes/Colloid-Orange-Dark-Compact-Catppuccin/gtk-4.0/gtk.css
    '';
  });
in
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
          monospace = [ "Hack" "Hack Nerd Font" ];
          sansSerif = [ "Noto Sans" "NotoSans Nerd Font" ];
          serif = [ "Noto Serif" "NotoSerif Nerd Font" ];
        };
      };
      packages = with pkgs; [
        hack-font
        nerd-fonts.hack
        nerd-fonts.noto
        noto-fonts
        noto-fonts-cjk-sans
        noto-fonts-color-emoji
        roboto
      ];
    };

    programs.dconf.enable = true;
    home-manager.users.charlotte = { pkgs, lib, ... }: {
      home.packages = [
        pkgs.catppuccin-cursors.latteLight
        # Also install dark mode to profile for darkman
        gtkTheme
      ];
      home.pointerCursor = {
        enable = true;
        package = pkgs.catppuccin-cursors.latteLight;
        dotIcons.enable = true;
        gtk.enable = true;
        x11.enable = true;
        name = "catppuccin-latte-light-cursors";
        size = 24;
      };
      dconf.settings."org/gnome/desktop/wm/preferences".button-layout = "";
      gtk = {
        enable = true;
        font = {
          package = pkgs.noto-fonts;
          name = "Noto Sans";
          size = 10;
        };
        gtk4.theme = {
          package = gtkTheme;
          name = "Colloid-Orange-Light-Compact-Catppuccin";
        };
        iconTheme = {
          package = pkgs.kdePackages.breeze-icons;
          name = "breeze";
        };
        theme = {
          package = gtkTheme;
          name = "Colloid-Orange-Light-Compact-Catppuccin";
        };
      };
      qt = {
        enable = true;
        platformTheme.name = "gtk3";
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
            mmsg dispatch spawn,"${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/color-scheme \"'prefer-dark'\""
            mmsg dispatch spawn,"${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme Colloid-Orange-Dark-Compact-Catppuccin"
          '';
          river = ''
            ln -sf ~/.config/mango/frappe.conf ~/.config/mango/theme.conf
            mmsg dispatch reload_config
          '';
          terminal = ''
            pkill -SIGUSR1 foot
          '';
        };
        lightModeScripts = {
          emacs = ''
            emacsclient --eval "(chvp--light-mode)"
          '';
          gtk = ''
            mmsg dispatch spawn,"${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/color-scheme \"'prefer-light'\""
            mmsg dispatch spawn,"${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme Colloid-Orange-Light-Compact-Catppuccin"
          '';
          river = ''
            ln -sf ~/.config/mango/latte.conf ~/.config/mango/theme.conf
            mmsg dispatch reload_config
          '';
          terminal = ''
            pkill -SIGUSR2 foot
          '';
        };
      };
    };
  };
}
