{
  config,
  lib,
  pkgs,
  unstablePkgs,
  ...
}:

let
  username = config.chvp.username;
  launcher = import ./launcher.nix {
    inherit pkgs;
    stdenv = pkgs.stdenv;
  };
  color-picker = import ./color-picker.nix { inherit pkgs unstablePkgs; };
  screenshot = import ./screenshot.nix { inherit pkgs; };
  lock = pkgs.writeShellScript "lock" ''
    if [ "$(${pkgs.darkman}/bin/darkman get)" == "light" ]
    then
      ${pkgs.swaylock}/bin/swaylock -fF -c eff1f5
    else
      ${pkgs.swaylock}/bin/swaylock -fF -c 303446
    fi
  '';
  baseWrapper = pkgs.writeShellScriptBin "mango" ''
    export XDG_SESSION_TYPE=wayland
    export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
    export QT_AUTO_SCREEN_SCALE_FACTOR=0
    export QT_SCALE_FACTOR=1
    export GDK_SCALE=1
    export GDK_DPI_SCALE=1
    export MOZ_ENABLE_WAYLAND=1
    export _JAVA_AWT_WM_NONREPARENTING=1
    if [ "$DBUS_SESSION_BUS_ADDRESS" ]; then
        export DBUS_SESSION_BUS_ADDRESS
        exec ${unstablePkgs.mango}/bin/mango
    else
        exec ${pkgs.dbus}/bin/dbus-run-session ${unstablePkgs.mango}/bin/mango
    fi
  '';
  mango = pkgs.symlinkJoin {
    name = "mango-${unstablePkgs.mango.version}";
    paths = [
      baseWrapper
      unstablePkgs.mango
    ];
    strictDeps = false;
    nativeBuildInputs = with pkgs; [
      makeWrapper
      wrapGAppsHook3
    ];
    buildInputs = with pkgs; [
      gdk-pixbuf
      glib
      gtk3
    ];
    dontWrapGApps = true;
    postBuild = ''
      gappsWrapperArgsHook

      wrapProgram $out/bin/mango "''${gappsWrapperArgs[@]}"
    '';
  };
  reload-config = ''
    if [ -d /run/user/$UID ]
    then
        WAYLAND_DISPLAY="$(${pkgs.findutils}/bin/find /run/user/$UID -mindepth 1 -maxdepth 1 -type s -name wayland-\*)"
        if [ -S "WAYLAND_DISPLAY" ]
        then
        ${unstablePkgs.mango}/bin/mmsg dispatch reload_config
        fi
    fi
  '';
  mango-config = ''
    # window effect
    blur = 0
    shadows = 0
    border_radius = 0
    focused_opacity = 1.0
    unfocused_opacity = 1.0
    animations = 0

    # appearance
    gappih = 0
    gappiv = 0
    gappoh = 0
    gappov = 0
    scratchpad_width_ratio = 0.8
    scratchpad_height_ratio = 0.9
    borderpx = 1
    source-optional = ./theme.conf

    # keyboard
    repeat_rate = 25
    repeat_delay = 600
    numlockon = 1
    xkb_rules_layout = us
    xkb_rules_variant = altgr-intl
    xkb_rules_options = compose:caps

    # touchpad
    tap_to_click = 1
    tap_and_drag = 1
    drag_lock = 1
    disable_while_typing = 1
    button_map = 0

    # layouts
    tagrule = id:1,layout_name:tile
    tagrule = id:2,layout_name:tile
    tagrule = id:3,layout_name:tile
    tagrule = id:4,layout_name:tile
    tagrule = id:5,layout_name:tile
    tagrule = id:6,layout_name:tile
    tagrule = id:7,layout_name:tile
    tagrule = id:8,layout_name:tile
    tagrule = id:9,layout_name:tile
    new_is_master = 0
    default_mfact = 0.60

    # keybinds
    bind = super,r,reload_config
    bind = super,Return,spawn,foot
    bind = super+shift,Return,spawn,emacs
    bind = super,d,spawn,${launcher}/bin/launcher
    bind = none,Menu,spawn,${launcher}/bin/launcher
    bind = none,XF86Calculator,spawn,foot zsh -ic qalc
    bind = super,c,spawn,${lock}

    bind = super+shift,c,killclient
    bind = super,f,zoom
    bind = super+shift,f,togglefullscreen
    bind = super,space,togglefloating
    bind = super+ctrl,f,togglefakefullscreen
    bind = super,j,focusstack,next
    bind = super,k,focusstack,previous
    bind = super+shift,j,exchange_stack_client,next
    bind = super+shift,k,exchange_stack_client,previous
    bind = super,h,focusmon,left
    bind = super,l,focusmon,right
    bind = super+shift,h,tagmon,left,1
    bind = super+shift,l,tagmon,right,1

    mousebind = super,btn_left,moveresize,curmove
    mousebind = super,btn_right,moveresize,curresize

    bind = none,XF86MonBrightnessUp,spawn,${pkgs.brightnessctl}/bin/brightnessctl s -- -5%
    bind = none,XF86MonBrightnessDown,spawn,${pkgs.brightnessctl}/bin/brightnessctl s -- -5%
    bind = none,XF86AudioRaiseVolume,spawn,wpctl set-volume @DEFAULT_SINK@ 5%+
    bind = none,XF86AudioLowerVolume,spawn,wpctl set-volume @DEFAULT_SINK@ 5%-
    bind = none,XF86AudioMute,spawn,wpctl set-mute @DEFAULT_SINK@ toggle
    bind = none,XF86AudioMicMute,spawn,wpctl set-mute @DEFAULT_SOURCE@ toggle
    bind = none,XF86AudioNext,spawn,${pkgs.playerctl}/bin/playerctl next
    bind = none,XF86AudioPlay,spawn,${pkgs.playerctl}/bin/playerctl play-pause
    bind = none,XF86AudioPrev,spawn,${pkgs.playerctl}/bin/playerctl previous

    bind = none,Print,spawn,${screenshot}/bin/screenshot
    bind = alt,Print,spawn,${screenshot}/bin/screenshot -d
    bind = shift,Print,spawn,${screenshot}/bin/screenshot -r
    bind = alt+shift,Print,spawn,${screenshot}/bin/screenshot -r -d
    bind = ctrl,Print,spawn,${screenshot}/bin/screenshot -f
    bind = ctrl+alt,Print,spawn,${screenshot}/bin/screenshot -f -d

    # tags
    bind = super,0,view,0,0
    bind = super,1,view,1,0
    bind = super,2,view,2,0
    bind = super,3,view,3,0
    bind = super,4,view,4,0
    bind = super,5,view,5,0
    bind = super,6,view,6,0
    bind = super,7,view,7,0
    bind = super,8,view,8,0
    bind = super,9,view,9,0
    bind = super+ctrl,1,toggleview,1,0
    bind = super+ctrl,2,toggleview,2,0
    bind = super+ctrl,3,toggleview,3,0
    bind = super+ctrl,4,toggleview,4,0
    bind = super+ctrl,5,toggleview,5,0
    bind = super+ctrl,6,toggleview,6,0
    bind = super+ctrl,7,toggleview,7,0
    bind = super+ctrl,8,toggleview,8,0
    bind = super+ctrl,9,toggleview,9,0
    bind = super+shift,0,tagsilent,1|2|3|4|5|6|7|8|9,0
    bind = super+shift,1,tagsilent,1,0
    bind = super+shift,2,tagsilent,2,0
    bind = super+shift,3,tagsilent,3,0
    bind = super+shift,4,tagsilent,4,0
    bind = super+shift,5,tagsilent,5,0
    bind = super+shift,6,tagsilent,6,0
    bind = super+shift,7,tagsilent,7,0
    bind = super+shift,8,tagsilent,8,0
    bind = super+shift,9,tagsilent,9,0
    bind = super+ctrl+shift,1,toggletag,1,0
    bind = super+ctrl+shift,2,toggletag,2,0
    bind = super+ctrl+shift,3,toggletag,3,0
    bind = super+ctrl+shift,4,toggletag,4,0
    bind = super+ctrl+shift,5,toggletag,5,0
    bind = super+ctrl+shift,6,toggletag,6,0
    bind = super+ctrl+shift,7,toggletag,7,0
    bind = super+ctrl+shift,8,toggletag,8,0
    bind = super+ctrl+shift,9,toggletag,9,0

    # misc
    no_border_when_single = 1
    cursor_hide_on_keypress = 1
    cursor_theme = ${config.home-manager.users.${username}.home.pointerCursor.name}
    cursor_size = ${toString config.home-manager.users.${username}.home.pointerCursor.size}

    exec-once = ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd DISPLAY XDG_SESSION_TYPE XCURSOR_SIZE QT_QPA_PLATFORM_THEME QT_STYLE_OVERRIDE QT_PLUGIN_PATH QTWEBKIT_PLUGIN_PATH GDK_PIXBUF_MODULE_FILE XDG_DATA_DIRS GIO_EXTRA_MODULES PATH
    exec-once = systemctl --user start mango-session.target
    exec-once = systemctl --user start tray.target
  '';
in
{
  options.chvp.graphical.compositor.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.compositor.enable {
    nixpkgs.overlays = [
      (self: super: {
        waybar = super.waybar.overrideAttrs (
          old:
          let
            libcava = rec {
              version = "1.0.0";
              src = pkgs.fetchFromGitHub {
                owner = "LukashonakV";
                repo = "cava";
                # NOTE: Needs to match the cava.wrap
                tag = "${version}";
                hash = "sha256-0r5aAmTs+FcmS501tNYKxG9H+Pq6i32BDRBEjWW6M74=";
              };
            };
          in
          {
            src = pkgs.fetchFromGitHub {
              owner = "Alexays";
              repo = "Waybar";
              rev = "084d87401d0a91182c16aa7e5f674a7dde767185";
              hash = "sha256-POvwObPOp6O14n6KYWNLp2Y3paunA5f8U1NCaodNFcc=";
            };
            postUnpack = ''
              pushd "$sourceRoot"
              cp -R --no-preserve=mode,ownership ${libcava.src} subprojects/cava-${libcava.version}
              patchShebangs .
              popd
            '';
            buildInputs = old.buildInputs ++ [ pkgs.modemmanager ];
          }
        );
      })
    ];
    services = {
      dbus.packages = with pkgs; [ dconf ];
      greetd = {
        enable = true;
        settings =
          let
            mango-run = pkgs.writeShellScript "mango-run" ''
              exec zsh -c "systemd-cat -t mango ${mango}/bin/mango"
            '';
          in
          {
            default_session = {
              command = "${pkgs.greetd}/bin/agreety --cmd ${mango-run}";
            };
            initial_session = {
              command = "${mango-run}";
              user = username;
            };
          };
      };
    };
    security.pam.services.swaylock.fprintAuth = true;
    xdg.portal = {
      enable = true;
      extraPortals = [
        pkgs.xdg-desktop-portal-gtk
        pkgs.xdg-desktop-portal-wlr
      ];
      config.preferred = {
        default = "gtk";
        "org.freedesktop.impl.portal.Screencast" = "wlr";
      };
    };
    home-manager.users.${username} = {
      home.packages = [
        mango
        color-picker
        screenshot
        pkgs.wl-clipboard
        pkgs.wl-mirror
      ];
      programs = {
        waybar = {
          enable = true;
          settings = {
            mainBar = {
              spacing = 2;
              modules-left = [ "mango/workspaces" ];
              modules-center = [ "mango/window" ];
              modules-right = [
                "idle_inhibitor"
                "network#wlp192s0"
                "battery"
                "backlight"
                "custom/notification"
                "pulseaudio"
                "clock"
                "tray"
              ];
              backlight = {
                format = "{percent}% {icon}";
                format-icons = [
                  "🌑"
                  "🌒"
                  "🌓"
                  "🌔"
                  "🌕"
                ];
                on-scroll-up = "${pkgs.brightnessctl}/bin/brightnessctl s -- +5%";
                on-scroll-down = "${pkgs.brightnessctl}/bin/brightnessctl s -- -5%";
              };
              battery = {
                states = {
                  good = 90;
                  warning = 30;
                  critical = 15;
                };
                format = "{capacity}% {icon}";
                format-charging = "{capacity}% ";
                format-plugged = "";
                format-alt = "{time} {icon}";
                format-icons = [
                  ""
                  ""
                  ""
                  ""
                  ""
                ];
              };
              clock.format = " {:%a %d/%m %H:%M}";
              "custom/notification" = {
                tooltip = true;
                format = "{icon} {0}";
                format-icons = {
                  notification = "󱅫";
                  none = "󰂜";
                  dnd-notification = "󰂠";
                  dnd-none = "󰪓";
                  inhibited-notification = "󰂛";
                  inhibited-none = "󰪑";
                  dnd-inhibited-notification = "󰂛";
                  dnd-inhibited-none = "󰪑";
                };
                return-type = "json";
                exec = "${pkgs.swaynotificationcenter}/bin/swaync-client -swb";
                on-click = "${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
                on-click-right = "${pkgs.swaynotificationcenter}/bin/swaync-client -d -sw";
                escape = true;
              };
              idle_inhibitor = {
                format = "{icon}";
                format-icons = {
                  activated = "";
                  deactivated = "";
                };
              };
              "network#wlp192s0" = {
                interface = "wlp192s0";
                format-wifi = "{essid} ";
                format-ethernet = "{ipaddr}/{cidr} ";
                tooltip-format = "{ifname} via {gwaddr} ";
                format-linked = "{ifname} (No IP) ";
                format-disconnected = "";
                format-alt = "{ifname}: {ipaddr}/{cidr}";
              };
              pulseaudio = {
                format = "{volume}% {icon} {format_source}";
                format-bluetooth = "{volume}% {icon} {format_source}";
                format-bluetooth-muted = " {icon} {format_source}";
                format-muted = " {format_source}";
                format-source = "{volume}% ";
                format-source-muted = "";
                format-icons = {
                  headphone = "";
                  hands-free = "";
                  headset = "";
                  phone = "";
                  portable = "";
                  car = "";
                  default = [
                    ""
                    ""
                    ""
                  ];
                };
                on-click = "${pkgs.pamixer}/bin/pamixer -t";
                on-click-right = "${pkgs.pamixer}/bin/pamixer --default-source -t";
                on-click-middle = "${pkgs.pavucontrol}/bin/pavucontrol";
              };
              "mango/workspaces" = {
                "on-click" = "activate";
                "on-click-right" = "toggle";
              };
              "mango/window".max-length = 30;
              tray.spacing = 2;
            };
          };
          style = ''
            * {
                font-family: "Noto Sans", sans-serif;
                font-size: 13px;
            }

            #window, #custom-notification, #idle_inhibitor, #network, #battery, #backlight, #mpris, #pulseaudio, #clock, #tray {
                margin: 0;
                padding: 0 5px;
                background-color: @catppuccin_surface0;
            }

            button {
                border: none;
                border-radius: 0;
            }
            button:hover {
                border: none;
                border-radius: 0;
            }

            window#waybar {
                background-color: @catppuccin_mantle;
                color: @catppuccin_text;
            }

            #backlight {
                color: @catppuccin_peach;
            }

            #battery {
                color: @catppuccin_green;
            }
            #battery.good {
                color: @catppuccin_peach;
            }
            #battery.warning {
                color: @catppuccin_yellow;
            }
            #battery.critical {
                color: @catppuccin_red;
            }

            #idle_inhibitor.activated {
                color: @catppuccin_sky;
            }

            #pulseaudio {
                color: @catppuccin_peach;
            }

            #workspaces button {
                padding: 0;
                box-shadow: inset 0 -2px transparent;
                background-color: @catppuccin_surface0;
            }
            #workspaces button.empty {
                background-color: transparent;
            }
            #workspaces button.active {
                color: @catppuccin_peach;
            }
            #workspaces button.urgent {
                color: @catppuccin_red;
            }
            #workspaces button:hover {
                box-shadow: inset 0 -2px @catppuccin_text;
            }
          '';
          systemd.enable = true;
        };
      };
      services = {
        kanshi = {
          enable = true;
          settings = [
            {
              profile = {
                name = "home-undocked";
                outputs = [
                  {
                    criteria = "BOE 0x0BCA";
                    position = "0,0";
                    mode = "2256x1504";
                    scale = 1.0;
                  }
                ];
              };
            }
            {
              profile = {
                name = "home-docked";
                outputs = [
                  {
                    criteria = "BOE 0x0BCA";
                    position = "0,0";
                    mode = "2256x1504";
                    scale = 1.0;
                  }
                  {
                    criteria = "LG Electronics LG ULTRAFINE 411NTJJ2F300";
                    position = "2256,0";
                    mode = "3840x2160";
                    scale = 1.0;
                  }
                ];
              };
            }
          ];
        };
        playerctld.enable = true;
        swayidle = {
          enable = true;
          events."before-sleep" = "${lock}";
          timeouts = [
            {
              timeout = 150;
              command = "${pkgs.wlopm}/bin/wlopm --off '*'";
              resumeCommand = "${pkgs.wlopm}/bin/wlopm --on '*'";
            }
            {
              timeout = 300;
              command = "${lock}";
            }
          ];
        };
        swaync = {
          enable = true;
          settings = {
            control-center-margin-top = 16;
            control-center-margin-bottom = 16;
            control-center-margin-right = 16;
            hide-on-action = false;
            widgets = [
              "mpris"
              "inhibitors"
              "title"
              "dnd"
              "notifications"
            ];
          };
        };
      };
      systemd.user.targets = {
        mango-session.Unit = {
          Description = "mango compositor session";
          BindsTo = [ "graphical-session.target" ];
          Wants = [ "graphical-session-pre.target" ];
          After = [ "graphical-session-pre.target" ];
        };
        tray.Unit = {
          Wants = [ "graphical-session.target" ];
          After = [ "graphical-session.target" ];
        };
      };
      xdg.configFile = {
        "mango/config.conf" = {
          text = mango-config;
          onChange = reload-config;
        };
        "mango/latte.conf" = {
          text = ''
            rootcolor = 0xeff1f5ff
            bordercolor = 0xdce0e8ff
            dropcolor = 0x8839ef80
            splitcolor = 0x179299ff
            focuscolor = 0xfe640bff
            maximizescreencolor = 0x40a02bff
            urgentcolor = 0xd20f39ff
            scratchpadcolor = 0x179299ff
            globalcolor = 0x7287fdff
            overlaycolor = 0xdf8e1dff
          '';
          onChange = reload-config;
        };
        "mango/frappe.conf" = {
          text = ''
            rootcolor = 0x303446ff
            bordercolor = 0x232634ff
            dropcolor = 0xca9ee680
            splitcolor = 0x81c8beff
            focuscolor = 0xef9f76ff
            maximizescreencolor = 0xa6d189ff
            urgentcolor = 0xe78284ff
            scratchpadcolor = 0x81c8beff
            globalcolor = 0xbabbf1ff
            overlaycolor = 0xe5c890ff
          '';
          onChange = reload-config;
        };
      };
      wayland.systemd.target = "mango-session.target";
    };
  };
}
