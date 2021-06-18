{ config, pkgs, ... }:
let
  launcher = import ./sway/launcher.nix { inherit pkgs; stdenv = pkgs.stdenv; };
  color-picker = import ./sway/color-picker.nix { inherit pkgs; };
  screenshot = import ./sway/screenshot.nix { inherit pkgs; };
  status-configuration = import ./sway/status-configuration.nix { inherit pkgs config; };
in
{
  config = {
    services.dbus.packages = with pkgs; [ gnome3.dconf ];
    security.pam.services.swaylock = { };
    xdg.portal = {
      enable = true;
      gtkUsePortal = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk pkgs.xdg-desktop-portal-wlr ];
    };
    home-manager.users.charlotte = { pkgs, lib, ... }: {
      home.packages = [ color-picker screenshot ];
      programs.mako = {
        enable = true;
        font = "Fira Code Normal 9";
      };
      services.kanshi = {
        enable = true;
        profiles = {
          "home-undocked" = {
            outputs = [
              { criteria = "Unknown 0x2036 0x00000000"; position = "0,0"; mode = "2560x1440"; scale = 1.0; }
            ];
          };
          "home-docked" = {
            outputs = [
              { criteria = "Unknown 0x2036 0x00000000"; position = "0,0"; mode = "2560x1440"; scale = 1.0; }
              { criteria = "Dell Inc. DELL U2718Q FN84K01T095L"; position = "2560,0"; mode = "3840x2160"; scale = 1.0; }
            ];
          };
          "work-undocked" = {
            outputs = [
              { criteria = "Chimei Innolux Corporation 0x14D3 0x00000000"; position = "0,0"; mode = "1920x1080"; scale = 1.0; }
            ];
          };
          "work-docked" = {
            outputs = [
              { criteria = "Chimei Innolux Corporation 0x14D3 0x00000000"; position = "0,0"; mode = "1920x1080"; scale = 1.0; }
              { criteria = "Dell Inc. DELL U2718Q FN84K83Q1KHL"; position = "1920,0"; mode = "3840x2160"; scale = 1.0; }
            ];
          };
        };
      };
      wayland.windowManager.sway = {
        enable = true;
        config = rec {
          modifier = "Mod4";
          left = "h";
          down = "j";
          up = "k";
          right = "l";
          terminal = "${pkgs.kitty}/bin/kitty";
          menu = "${terminal} --class launcher -e ${launcher}/bin/launcher";
          fonts = { names = [ "Fira Code" ]; size = 9.0; style = "Normal"; };
          bars = [
            {
              colors = {
                background = "#ffffff";
                statusline = "#000000";
                activeWorkspace = { border = "#f2eff3"; background = "#f2eff3"; text = "#000000"; };
                focusedWorkspace = { border = "#6aaeff"; background = "#6aaeff"; text = "#000000"; };
                inactiveWorkspace = { border = "#ffffff"; background = "#ffffff"; text = "#000000"; };
                urgentWorkspace = { border = "#ff8892"; background = "#ff8892"; text = "#000000"; };
              };
              fonts = { names = [ "Fira Code" ]; size = 9.0; style = "Normal"; };
              position = "top";
              statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ${status-configuration}";
              extraConfig = ''
                status_padding 0
                icon_theme Arc
              '';
            }
          ];
          output = {
            "Unknown 0x2036 0x00000000" = { position = "0,0"; mode = "2560x1440"; scale = "1.0"; };
            "Dell Inc. DELL U2718Q FN84K01T095L" = { position = "2560,0"; mode = "3840x2160"; scale = "1.0"; };
            "Chimei Innolux Corporation 0x14D3 0x00000000" = { position = "0,0"; mode = "1920x1080"; scale = "1.0"; };
            "Dell Inc. DELL U2718Q FN84K83Q1KHL" = { position = "1920,0"; mode = "3840x2160"; scale = "1.0"; };
          };
          startup = [
            {
              command = "${pkgs.swayidle}/bin/swayidle -w timeout 300 '${pkgs.swaylock}/bin/swaylock -f -c 000000' timeout 150 '${pkgs.sway}/bin/swaymsg \"output * dpms off\"' resume '${pkgs.sway}/bin/swaymsg \"output * dpms on\"' before-sleep '${pkgs.swaylock}/bin/swaylock -f -c 000000'";
            }
          ];
          window.commands = [
            { command = "floating enable"; criteria = { app_id = "launcher"; }; }
            { command = "floating enable"; criteria = { title = "Quick Format Citation"; class = "Zotero"; }; }
          ];
          input = {
            "type:keyboard" = { xkb_layout = "us"; xkb_variant = "altgr-intl"; xkb_numlock = "enabled"; xkb_options = "compose:caps"; };
            "type:touchpad" = { drag = "enabled"; dwt = "enabled"; scroll_method = "two_finger"; tap = "enabled"; };
          };
          modes = { }; # Unset default "resize" mode
          keybindings = lib.mkOptionDefault {
            "${modifier}+Shift+q" = "nop Unset default kill";
            "${modifier}+r" = "nop Unset default resize mode";
            "${modifier}+Shift+c" = "kill";
            "${modifier}+Shift+r" = "reload";
            "${modifier}+c" = "exec ${pkgs.swaylock}/bin/swaylock -f -c 000000";
            "${modifier}+i" = "inhibit_idle open; border normal; mark --add inhibiting_idle";
            "${modifier}+Shift+i" = "inhibit_idle none; border pixel; unmark inhibiting_idle";
            "Print" = "exec ${screenshot}/bin/screenshot";
            "Alt+Print" = "exec ${screenshot}/bin/screenshot -d";
            "Shift+Print" = "exec ${screenshot}/bin/screenshot -r";
            "Alt+Shift+Print" = "exec ${screenshot}/bin/screenshot -r -d";
            "XF86AudioRaiseVolume" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%";
            "XF86AudioLowerVolume" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%";
            "XF86AudioMute" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
            "XF86AudioMicMute" = "exec ${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle";
            "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 5%-";
            "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set +5%";
            "${modifier}+Alt+Left" = "move workspace to output left";
            "${modifier}+Alt+Right" = "move workspace to output right";
          };
        };
        extraConfig = ''
          workspace 1 output eDP-1
          workspace 2 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
          workspace 3 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
          workspace 4 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
          workspace 5 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
          workspace 6 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
          workspace 7 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
          workspace 8 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
          workspace 9 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1

          workspace 1
          exec ${pkgs.firefox}/bin/firefox

          no_focus [title="Microsoft Teams Notification"]

          default_border pixel
        '';
        extraSessionCommands = ''
          export XDG_SESSION_TYPE=wayland
          export XDG_CURRENT_DESKTOP=sway
          export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
          export QT_AUTO_SCREEN_SCALE_FACTOR=0
          export QT_SCALE_FACTOR=1
          export GDK_SCALE=1
          export GDK_DPI_SCALE=1
          export MOZ_ENABLE_WAYLAND=1
          export _JAVA_AWT_WM_NONREPARENTING=1
        '';
        wrapperFeatures = {
          base = true;
          gtk = true;
        };
        xwayland = true;
      };
    };
  };
}
