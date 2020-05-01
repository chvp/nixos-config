with import <nixpkgs> { };
{ pkgs, ... }:
let
  firefox = import ../firefox/default.nix { inherit pkgs; };
  launcher = import ./launcher.nix { inherit pkgs stdenv; };
  color-picker = import ./color-picker.nix { inherit pkgs; };
  screenshot = import ./screenshot.nix { inherit pkgs; };
  status-configuration = import ./status-configuration.nix { inherit pkgs; };
in
{
  imports = [
    ./kanshi.nix
  ];
  programs = {
    sway = {
      enable = true;
      extraPackages = [ pkgs.xwayland ];
      extraSessionCommands = ''
        export XDG_SESSION_TYPE=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
        export QT_AUTO_SCREEN_SCALE_FACTOR=0
        export QT_SCALE_FACTOR=1
        export GDK_SCALE=1
        export GDK_DPI_SCALE=1
        export MOZ_ENABLE_WAYLAND=1
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
    };
  };

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ color-picker launcher screenshot ];
    xdg.configFile."sway/config".text = ''
      # Config for sway
      #
      # Read `man 5 sway` for a complete reference.

      ### Variables
      #
      # Logo key. Use Mod1 for Alt.
      set $mod Mod4
      # Home row direction keys, like vim
      set $left h
      set $down j
      set $up k
      set $right l
      # Your preferred terminal emulator
      set $term ${pkgs.kitty}/bin/kitty
      # Your preferred application launcher
      # Note: it's recommended that you pass the final command to sway
      set $menu $term --class launcher -e ${launcher}/bin/launcher

      ### Output configuration
      exec_always pkill kanshi; exec ${pkgs.kanshi}/bin/kanshi

      workspace 1 output eDP-1
      workspace 2 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
      workspace 3 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
      workspace 4 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
      workspace 5 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
      workspace 6 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
      workspace 7 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
      workspace 8 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
      workspace 9 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1
      workspace 10 output DP-3 DP-4 DP-5 HDMI-A-1 eDP-1

      ### Idle configuration
      #
      # This will lock your screen after 300 seconds of inactivity, turn off
      # your displays after another 150 seconds, and turn your screens back on when
      # resumed. It will also lock your screen before your computer goes to sleep.
      exec ${pkgs.swayidle}/bin/swayidle -w \
              timeout 300 '${pkgs.swaylock}/bin/swaylock -f -c 000000' \
              timeout 150 '${pkgs.sway}/bin/swaymsg "output * dpms off"' \
                   resume '${pkgs.sway}/bin/swaymsg "output * dpms on"' \
             before-sleep '${pkgs.swaylock}/bin/swaylock -f -c 000000'

      ### Notification daemon
      #
      exec ${pkgs.mako}/bin/mako

      # User services bound to the graphical session
      exec "${pkgs.systemd}/bin/systemctl --user import-environment; ${pkgs.systemd}/bin/systemctl --user start graphical-session.target"

      ### Window rules
      # Launcher popup
      for_window [app_id="launcher"] floating enable

      # Start accentor as floating window
      for_window [class="accentor.Main"] floating enable

      ### Startup programs
      #
      workspace 1
      exec ${firefox}/bin/firefox
      workspace 3
      exec ${pkgs.thunderbird}/bin/thunderbird
      workspace 4
      exec ${pkgs.joplin-desktop}/bin/joplin-desktop
      workspace 6
      exec ${pkgs.teams}/bin/teams
      workspace 2

      ### Input configuration
      #
      # Example configuration:
      #
      #   input "2:14:SynPS/2_Synaptics_TouchPad" {
      #       dwt enabled
      #       tap enabled
      #       natural_scroll enabled
      #       middle_emulation enabled
      #   }
      #
      # You can get the names of your inputs by running: swaymsg -t get_inputs
      # Read `man 5 sway-input` for more information about this section.

      input type:keyboard {
        xkb_layout "us"
        xkb_variant "altgr-intl"
        xkb_numlock enabled
      }

      input type:touchpad {
        drag enabled
        dwt enabled
        scroll_method two_finger
        tap enabled
      }

      ### Key bindings
      #
      # Basics:
      #
      # start a terminal
      bindsym $mod+Return exec $term

      # kill focused window
      bindsym $mod+Shift+c kill

      # start your launcher
      bindsym $mod+d exec $menu

      # Drag floating windows by holding down $mod and left mouse button.
      # Resize them with right mouse button + $mod.
      # Despite the name, also works for non-floating windows.
      # Change normal to inverse to use left mouse button for resizing and right
      # mouse button for dragging.
      floating_modifier $mod normal

      # reload the configuration file
      bindsym $mod+Shift+r reload

      # exit sway (logs you out of your Wayland session)
      bindsym $mod+Shift+e exec ${pkgs.sway}/bin/swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' '${pkgs.sway}/bin/swaymsg exit'

      # lock screen
      bindsym $mod+c exec ${pkgs.swaylock}/bin/swaylock -f -c 000000

      # toggle the current view inhibiting idle
      bindsym $mod+i inhibit_idle open; border normal; mark --add inhibiting_idle
      bindsym $mod+Shift+i inhibit_idle none; border pixel; unmark inhibiting_idle


      # screenshot
      bindsym Print exec ${screenshot}/bin/screenshot
      bindsym Alt+Print exec ${screenshot}/bin/screenshot -d
      bindsym Shift+Print exec ${screenshot}/bin/screenshot -r
      bindsym Alt+Shift+Print exec ${screenshot}/bin/screenshot -r -d

      # audio
      bindsym XF86AudioRaiseVolume exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%
      bindsym XF86AudioLowerVolume exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%
      bindsym XF86AudioMute exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle
      bindsym XF86AudioMicMute exec ${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle

      # brightness
      bindsym XF86MonBrightnessDown exec ${pkgs.brightnessctl}/bin/brightnessctl set 5%-
      bindsym XF86MonBrightnessUp exec ${pkgs.brightnessctl}/bin/brightnessctl set +5%
      #
      # Moving around:
      #
      # Move your focus around
      bindsym $mod+$left focus left
      bindsym $mod+$down focus down
      bindsym $mod+$up focus up
      bindsym $mod+$right focus right
      # or use $mod+[up|down|left|right]
      bindsym $mod+Left focus left
      bindsym $mod+Down focus down
      bindsym $mod+Up focus up
      bindsym $mod+Right focus right

      # _move_ the focused window with the same, but add Shift
      bindsym $mod+Shift+$left move left
      bindsym $mod+Shift+$down move down
      bindsym $mod+Shift+$up move up
      bindsym $mod+Shift+$right move right
      # ditto, with arrow keys
      bindsym $mod+Shift+Left move left
      bindsym $mod+Shift+Down move down
      bindsym $mod+Shift+Up move up
      bindsym $mod+Shift+Right move right
      #
      # Workspaces:
      #
      # switch to workspace
      bindsym $mod+1 workspace 1
      bindsym $mod+2 workspace 2
      bindsym $mod+3 workspace 3
      bindsym $mod+4 workspace 4
      bindsym $mod+5 workspace 5
      bindsym $mod+6 workspace 6
      bindsym $mod+7 workspace 7
      bindsym $mod+8 workspace 8
      bindsym $mod+9 workspace 9
      bindsym $mod+0 workspace 10
      # move focused container to workspace
      bindsym $mod+Shift+1 move container to workspace 1
      bindsym $mod+Shift+2 move container to workspace 2
      bindsym $mod+Shift+3 move container to workspace 3
      bindsym $mod+Shift+4 move container to workspace 4
      bindsym $mod+Shift+5 move container to workspace 5
      bindsym $mod+Shift+6 move container to workspace 6
      bindsym $mod+Shift+7 move container to workspace 7
      bindsym $mod+Shift+8 move container to workspace 8
      bindsym $mod+Shift+9 move container to workspace 9
      bindsym $mod+Shift+0 move container to workspace 10
      # move workspace to output
      bindsym $mod+Alt+Left move workspace to output left
      bindsym $mod+Alt+Right move workspace to output right
      # Note: workspaces can have any name you want, not just numbers.
      # We just use 1-10 as the default.

      #
      # Layout stuff:
      #
      # You can "split" the current object of your focus with
      # $mod+b or $mod+v, for horizontal and vertical splits
      # respectively.
      bindsym $mod+b splith
      bindsym $mod+v splitv

      # Switch the current container between different layout styles
      bindsym $mod+s layout stacking
      bindsym $mod+w layout tabbed
      bindsym $mod+e layout toggle split

      # Make the current focus fullscreen
      bindsym $mod+f fullscreen

      # Toggle the current focus between tiling and floating mode
      bindsym $mod+Shift+space floating toggle

      # Swap focus between the tiling area and the floating area
      bindsym $mod+space focus mode_toggle

      # move focus to the parent container
      bindsym $mod+a focus parent

      #
      # Status Bar:
      #
      # Read `man 5 sway-bar` for more information about this section.
      bar {
        position top

        status_command ${pkgs.i3status-rust}/bin/i3status-rs ${status-configuration}
        status_padding 0

        font Fira Code Normal 9

        colors {
          statusline #535c65
          background #fbffff
          focused_workspace #2b7ab2 #2b7ab2 #fbffff
          active_workspace #6d7782 #6d7782 #fbffff
          inactive_workspace #fbffff #fbffff #535c65
          urgent_workspace #ae5865 #ae5865 #fbffff
        }

        icon_theme Breeze
      }

      default_border pixel

      include /etc/sway/config.d/*
    '';
  };
}
