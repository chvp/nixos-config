with import <nixpkgs> {};
{ pkgs, ... }:

let
  gemoji = pkgs.buildRubyGem {
    pname = "gemoji";
    gemName = "gemoji";
    source.sha256 = "1xv38sxql1fmaxi5lzj6v98l2aqhi6bqkhi6kqd0k38vw40l3yqc";
    type = "gem";
    version = "4.0.0.rc2";
  };

  emoji_list = stdenv.mkDerivation {
    name = "emoji_list";
    buildInputs = [ pkgs.ruby gemoji ];
    unpackPhase = "true";
    buildPhase = ''
      cat > extract_emoji.rb <<HERE
      require 'emoji'
      File.open('emoji_list.txt', 'w') do |f|
        Emoji.all.each do |e|
          f.puts("#{e.raw} #{e.description} #{e.name}#{(" " + e.tags.join(" ")) if e.tags.any?} (#{e.category})")
        end
      end
      HERE
      ruby extract_emoji.rb
    '';
    installPhase = ''
      cp emoji_list.txt $out
    '';
  };

  launcher = pkgs.writeScript "launcher" ''
    #!${pkgs.zsh}/bin/zsh

    _sighandler() {
      kill -INT "$child" 2>/dev/null
    }

    calc_options() {
      echo "calc "
    }

    calc() {
      if [ -n "$1" ]
      then
        ${pkgs.libqalculate}/bin/qalc "$1"
        sleep 5
      else
        ${pkgs.libqalculate}/bin/qalc
      fi
    }

    emoji_options() {
      cat ${emoji_list} | sed "s/^/emoji /"
    }

    emoji() {
      char=$(echo -n "$1" | sed "s/^\([^ ]*\) .*/\1/")
      ${pkgs.sway}/bin/swaymsg exec -- "echo -n $char | ${pkgs.wl-clipboard}/bin/wl-copy --foreground"
    }

    record_options() {
      ${pkgs.sway}/bin/swaymsg -t get_outputs | ${pkgs.jq}/bin/jq -r '.[]["name"]' | sed "s/^/record /"
      echo record select
    }

    record() {
      filename="$(${xdg-user-dirs}/bin/xdg-user-dir VIDEOS)/$(date +'screenrecording_%y-%m-%d-%H%M%S.mp4')"

      trap _sighandler SIGINT
      if [[ "$1" = "select" ]]
      then
        ${pkgs.wf-recorder}/bin/wf-recorder -g "$(${pkgs.slurp}/bin/slurp)" -f "$filename" &
      else
        wf-recorder -o $! -f "$filename" &
      fi
      child=$!
      wait "$child"
      # We wait two times, because the first wait exits when the process receives a signal. The process might have finished though, so we ignore errors.
      wait "$child" 2>/dev/null
      if [ -f "$filename" ]
      then
        echo "Saved as $filename"
      else
        echo "Something went wrong while recording"
      fi
      sleep 5
    }

    run_options() {
      print -rl -- ''${(ko)commands} | sed "s/^/run /"
    }

    run() {
      ${pkgs.sway}/bin/swaymsg exec $1
    }

    ssh_options() {
      cat $HOME/.ssh/config | grep "^Host [a-zA-Z]\+" | sed "s/Host /ssh /"
    }

    ssh() {
      ${pkgs.sway}/bin/swaymsg exec "${pkgs.kitty}/bin/kitty -e ssh $1"
    }

    windows_options() {
      ${pkgs.sway}/bin/swaymsg -t get_tree | ${pkgs.jq}/bin/jq -r 'recurse(.nodes[]?)|recurse(.floating_nodes[]?)|select(.layout=="none")|select(.app_id!="launcher")|select(.type=="con"),select(.type=="floating_con")|(if .app_id then .app_id else .window_properties.class end)+": "+.name+" ("+(.id|tostring)+")"' | sed "s/^/windows /"
    }

    windows() {
      window=$(echo $@ | sed 's/.* (\([^)]*\))$/\1/')
      ${pkgs.sway}/bin/swaymsg \[con_id="$window"\] focus
    }

    CHOSEN=$(cat <(windows_options) <(ssh_options) <(run_options) <(record_options) <(calc_options) <(emoji_options) | ${pkgs.fzy}/bin/fzy --lines 36 | tail -n1)

    if [ -n "$CHOSEN" ]
    then
      PREFIX=$(echo $CHOSEN | sed "s/^\([^ ]*\) .*/\1/g")
      WORD=$(echo $CHOSEN | sed "s/^[^ ]* \(.*\)/\1/g")

      $PREFIX $WORD
    fi
  '';
in
  {
    imports = [
      ./kanshi.nix
      ./waybar.nix
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
      nixpkgs.config.packageOverrides = pkgs: {
        waybar = pkgs.waybar.override { pulseSupport = true; mpdSupport = false; };
      };
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
        set $menu $term --class launcher -e ${launcher}

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
        #
        # Common programs
        assign [app_id="firefox"] 1
        assign [app_id="thunderbird"] 3
        assign [class="rambox"] 4

        # Launcher popup
        for_window [app_id="launcher"] floating enable

        ### Startup programs
        #
        exec ${pkgs.firefox}/bin/firefox
        exec ${pkgs.thunderbird}/bin/thunderbird
        exec ${pkgs.rambox}/bin/rambox

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

        input "1:1:AT_Translated_Set_2_keyboard" {
          xkb_layout "us"
          xkb_variant "altgr-intl"
          xkb_numlock enabled
          xkb_options "compose:caps"
        }

        input "1241:513:USB-HID_Keyboard" {
          xkb_layout "us"
          xkb_variant "altgr-intl"
          xkb_numlock enabled
          xkb_options "compose:caps"
        }

        input "2:7:SynPS/2_Synaptics_TouchPad" {
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

        # screenshot
        bindsym Print exec ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" $(${pkgs.xdg-user-dirs}/bin/xdg-user-dir PICTURES)/$(${pkgs.coreutils}/bin/date +'screenshot_%Y-%m-%d-%H%M%S.png')

        # audio
        bindsym XF86AudioRaiseVolume exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume $(${pkgs.pulseaudio}/bin/pacmd list-sinks |${pkgs.gawk}/bin/awk '/* index:/{print $3}') +5%
        bindsym XF86AudioLowerVolume exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume $(${pkgs.pulseaudio}/bin/pacmd list-sinks |${pkgs.gawk}/bin/awk '/* index:/{print $3}') -5%
        bindsym XF86AudioMute exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute $(${pkgs.pulseaudio}/bin/pacmd list-sinks |${pkgs.gawk}/bin/awk '/* index:/{print $3}') toggle

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
        # Scratchpad:
        #
        # Sway has a "scratchpad", which is a bag of holding for windows.
        # You can send windows there and get them back later.

        # Move the currently focused window to the scratchpad
        bindsym $mod+Shift+minus move scratchpad

        # Show the next scratchpad window or hide the focused scratchpad window.
        # If there are multiple scratchpad windows, this command cycles through them.
        bindsym $mod+minus scratchpad show

        #
        # Status Bar:
        #
        # Read `man 5 sway-bar` for more information about this section.
        bar {
          swaybar_command ${pkgs.waybar}/bin/waybar
        }

        default_border pixel

        include /etc/sway/config.d/*
      '';
    };
  }
