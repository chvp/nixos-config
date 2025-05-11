{ config, lib, pkgs, ... }:

let
  launcher = import ./launcher.nix { inherit pkgs; stdenv = pkgs.stdenv; };
  color-picker = import ./color-picker.nix { inherit pkgs; };
  screenshot = import ./screenshot.nix { inherit pkgs; };
  mail-status = pkgs.writeShellScript "mail-status" ''
    mails=$(${pkgs.mblaze}/bin/mlist -N ~/mail/*/INBOX | ${pkgs.coreutils}/bin/wc -l)
    if [ "$mails" -gt 0 ]
    then
      echo "{ \"class\": \"has-mail\", \"text\": \"📬 $mails\" }"
    else
      echo "{ \"text\": \"📭\" }"
    fi
  '';
  lock = pkgs.writeShellScript "lock" ''
    if [ "$(darkman get)" == "light" ]
    then
      ${pkgs.swaylock}/bin/swaylock -fF -c eff1f5
    else
      ${pkgs.swaylock}/bin/swaylock -fF -c 303446
    fi
  '';
  baseWrapper = pkgs.writeShellScriptBin "river" ''
    export XDG_SESSION_TYPE=wayland
    export XDG_CURRENT_DESKTOP=river
    export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
    export QT_AUTO_SCREEN_SCALE_FACTOR=0
    export QT_SCALE_FACTOR=1
    export GDK_SCALE=1
    export GDK_DPI_SCALE=1
    export MOZ_ENABLE_WAYLAND=1
    export _JAVA_AWT_WM_NONREPARENTING=1
    if [ "$DBUS_SESSION_BUS_ADDRESS" ]; then
        export DBUS_SESSION_BUS_ADDRESS
        exec ${pkgs.river}/bin/river
    else
        exec ${pkgs.dbus}/bin/dbus-run-session ${pkgs.river}/bin/river
    fi
  '';
  river = pkgs.symlinkJoin {
    name = "river-${pkgs.river.version}";
    paths = [ baseWrapper pkgs.river ];
    strictDeps = false;
    nativeBuildInputs = with pkgs; [ makeWrapper wrapGAppsHook ];
    buildInputs = with pkgs; [ gdk-pixbuf glib gtk3 ];
    dontWrapGApps = true;
    postBuild = ''
      gappsWrapperArgsHook

      wrapProgram $out/bin/river "''${gappsWrapperArgs[@]}"
    '';
  };
  river-init = pkgs.writeShellScript "river-init" ''
    riverctl map normal Super Return spawn foot
    riverctl map normal Super D spawn 'foot --app-id launcher -- zsh -ic ${launcher}/bin/launcher'
    riverctl map normal None Menu spawn 'foot --app-id launcher -- zsh -ic ${launcher}/bin/launcher'

    riverctl map normal Super C spawn ${lock}

    riverctl map normal Super+Shift C close

    riverctl map normal Super+Shift E exit

    riverctl map normal Super J focus-view next
    riverctl map normal Super K focus-view previous

    riverctl map normal Super+Shift J swap next
    riverctl map normal Super+Shift K swap previous

    riverctl map normal Super H focus-output next
    riverctl map normal Super L focus-output previous

    riverctl map normal Super+Shift H send-to-output -current-tags next
    riverctl map normal Super+Shift L send-to-output -current-tags previous

    riverctl map normal Super F zoom
    riverctl map normal Super+Shift F toggle-fullscreen

    riverctl map normal Super+Control H send-layout-cmd rivertile "main-ratio -0.05"
    riverctl map normal Super+Control L send-layout-cmd rivertile "main-ratio +0.05"

    riverctl map normal Super+Control+Shift H send-layout-cmd rivertile "main-count +1"
    riverctl map normal Super+Control+Shift L send-layout-cmd rivertile "main-count -1"

    riverctl map normal Super+Alt H move left 100
    riverctl map normal Super+Alt J move down 100
    riverctl map normal Super+Alt K move up 100
    riverctl map normal Super+Alt L move right 100

    riverctl map normal Super+Alt+Control H snap left
    riverctl map normal Super+Alt+Control J snap down
    riverctl map normal Super+Alt+Control K snap up
    riverctl map normal Super+Alt+Control L snap right

    riverctl map normal Super+Alt+Shift H resize horizontal -100
    riverctl map normal Super+Alt+Shift J resize vertical 100
    riverctl map normal Super+Alt+Shift K resize vertical -100
    riverctl map normal Super+Alt+Shift L resize horizontal 100

    riverctl map normal Super Space toggle-float

    riverctl map-pointer normal Super BTN_LEFT move-view
    riverctl map-pointer normal Super BTN_RIGHT resize-view

    for i in $(seq 1 9)
    do
      tags=$((1 << ($i - 1)))
      riverctl map normal Super $i set-focused-tags $tags
      riverctl map normal Super+Shift $i set-view-tags $tags
      riverctl map normal Super+Control $i toggle-focused-tags $tags
      riverctl map normal Super+Shift+Control $i toggle-view-tags $tags
    done

    all_tags=$(((1 << 32) - 1))
    riverctl map normal Super 0 set-focused-tags $all_tags
    riverctl map normal Super+Shift 0 set-view-tags $all_tags

    riverctl map normal Super Up    send-layout-cmd rivertile "main-location top"
    riverctl map normal Super Right send-layout-cmd rivertile "main-location right"
    riverctl map normal Super Down  send-layout-cmd rivertile "main-location bottom"
    riverctl map normal Super Left  send-layout-cmd rivertile "main-location left"

    riverctl map normal None XF86AudioRaiseVolume spawn '${pkgs.pamixer}/bin/pamixer -i 5'
    riverctl map normal None XF86AudioLowerVolume spawn '${pkgs.pamixer}/bin/pamixer -d 5'
    riverctl map normal None XF86AudioMute spawn '${pkgs.pamixer}/bin/pamixer --toggle-mute'
    riverctl map normal None XF86AudioPlay spawn '${pkgs.playerctl}/bin/playerctl play-pause'
    riverctl map normal None XF86MonBrightnessDown spawn '${pkgs.brightnessctl}/bin/brightnessctl s -- -5%'
    riverctl map normal None XF86MonBrightnessUp spawn '${pkgs.brightnessctl}/bin/brightnessctl s -- +5%'

    riverctl map normal None Print spawn '${screenshot}/bin/screenshot'
    riverctl map normal Alt Print spawn '${screenshot}/bin/screenshot -d'
    riverctl map normal Shift Print spawn '${screenshot}/bin/screenshot -r'
    riverctl map normal Alt+Shift Print spawn '${screenshot}/bin/screenshot -r -d'

    riverctl rule-add -app-id launcher float
    riverctl rule-add -app-id be.ugent.objprog.ugentopoly.Ugentopoly float
    riverctl rule-add -title "Quick Format Citation" float

    riverctl rule-add ssd

    riverctl default-layout rivertile
    rivertile -view-padding 0 -outer-padding 0 &

    riverctl default-attach-mode bottom
    riverctl background-color 0xacb0be
    riverctl border-color-focused 0x04e5e5
    riverctl border-color-unfocused 0xdce0e8
    riverctl border-color-urgent 0xea76cb
    riverctl border-width 1
    riverctl focus-follows-cursor normal
    riverctl hide-cursor when-typing enabled
    riverctl set-cursor-warp on-output-change
    riverctl xcursor-theme ${config.home-manager.users.charlotte.home.pointerCursor.name} ${toString config.home-manager.users.charlotte.home.pointerCursor.size}

    riverctl keyboard-layout -variant altgr-intl -options compose:caps us

    configure_touchpads() {
      riverctl list-inputs | grep 'type: pointer' -B 1 | grep -vE 'type: pointer|^--$' | xargs -I '{}' riverctl input '{}' $@
    }

    configure_touchpads drag enabled
    configure_touchpads tap enabled
    configure_touchpads scroll-method two-finger

    ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY XDG_CURRENT_DESKTOP XDG_SESSION_TYPE XCURSOR_SIZE QT_QPA_PLATFORM_THEME QT_STYLE_OVERRIDE QT_PLUGIN_PATH QTWEBKIT_PLUGIN_PATH GDK_PIXBUF_MODULE_FILE XDG_DATA_DIRS GIO_EXTRA_MODULES PATH
    systemctl --user start river-session.target
    systemctl --user start tray.target
  '';
in
{
  options.chvp.graphical.compositor.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.compositor.enable {
    services = {
      dbus.packages = with pkgs; [ dconf ];
      greetd = {
        enable = true;
        settings =
          let
            river-run = pkgs.writeShellScript "river-run" ''
              exec zsh -c "systemd-cat -t river ${river}/bin/river"
            '';
          in
          {
            default_session = {
              command = "${pkgs.greetd.greetd}/bin/agreety --cmd ${river-run}";
            };
            initial_session = {
              command = "${river-run}";
              user = "charlotte";
            };
          };
      };
    };
    security.pam.services.swaylock = { };
    xdg.portal = {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk pkgs.xdg-desktop-portal-wlr ];
      config.preferred = {
        default = "gtk";
        "org.freedesktop.impl.portal.Screencast" = "wlr";
      };
    };
    home-manager.users.charlotte = { ... }: {
      home.packages = [
        river
        color-picker
        screenshot
        # pkgs.wf-recorder
        pkgs.wl-clipboard
        pkgs.wl-mirror
      ];
      programs = {
        waybar = {
          enable = true;
          settings = {
            mainBar = {
              spacing = 2;
              modules-left = [ "river/tags" ];
              modules-center = [ "river/window" ];
              modules-right = [ "idle_inhibitor" "network#wlp0s20f3" "network#enp0s13f0u2u2" "battery" "backlight" "mpris" "pulseaudio" "custom/mail-status" "clock" "tray" ];
              backlight = {
                format = "{percent}% {icon}";
                format-icons = [ "🌑" "🌒" "🌓" "🌔" "🌕" ];
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
                format-icons = [ "" "" "" "" "" ];
              };
              clock.format = " {:%a %d/%m %H:%M}";
              "custom/mail-status" = {
                exec = "${mail-status}";
                return-type = "json";
                interval = 1;
                on-click = "${pkgs.isync}/bin/mbsync -a && ${config.chvp.base.emacs.package}/bin/emacsclient --eval \"(mu4e-update-index)\"";
              };
              idle_inhibitor = {
                format = "{icon}";
                format-icons = {
                  activated = "";
                  deactivated = "";
                };
              };
              mpris = {
                player = "firefox";
                format = "{status_icon} {artist} - {title}";
                status-icons = {
                  playing = "▶";
                  paused = "";
                  stopped = "";
                };
              };
              "network#wlp0s20f3" = {
                interface = "wlp0s20f3";
                format-wifi = "{essid} ";
                format-ethernet = "{ipaddr}/{cidr} ";
                tooltip-format = "{ifname} via {gwaddr} ";
                format-linked = "{ifname} (No IP) ";
                format-disconnected = "";
                format-alt = "{ifname}: {ipaddr}/{cidr}";
              };
              "network#enp0s13f0u2u2" = {
                interface = "enp0s13f0u2u2";
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
                  default = [ "" "" "" ];
                };
                on-click = "${pkgs.pamixer}/bin/pamixer -t";
                on-click-right = "${pkgs.pamixer}/bin/pamixer --default-source -t";
                on-click-middle = "${pkgs.pavucontrol}/bin/pavucontrol";
              };
              tray.spacing = 2;
            };
          };
          style = ''
            @import "colors.css";

            * {
                font-family: Hack, monospace;
                font-size: 11px;
            }

            #window, #idle_inhibitor, #network, #battery, #backlight, #mpris, #pulseaudio, #custom-mail-status, #clock, #tray {
                margin: 0;
                padding: 0 5px;
                background-color: @surface0;
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
                background-color: @base;
                color: @text;
            }

            #backlight {
                color: @sky;
            }

            #battery {
                color: @green;
            }
            #battery.good {
                color: @sky;
            }
            #battery.warning {
                color: @yellow;
            }
            #battery.critical {
                color: @pink;
            }

            #custom-mail-status.has-mail {
                color: @sky;
            }

            #idle_inhibitor.activated {
                color: @sky;
            }

            #pulseaudio {
                color: @yellow;
            }

            #tags button {
                padding: 0;
                box-shadow: inset 0 -3px transparent
                color: @text;
            }
            #tags button.occupied {
                background-color: @surface1;
            }
            #tags button.focused {
                color: @sky;
            }
            #tags button.urgent {
                color: @pink;
            }
            #tags button:hover {
                box-shadow: inset 0 -3px @text;
            }
          '';
          systemd.enable = true;
        };
        zsh.initContent = ''
          rs() {
            riverctl spawn "$*"
          }
        '';
      };
      services = {
        kanshi = {
          enable = true;
          systemdTarget = "river-session.target";
          settings = [
            {
              profile = {
                name = "home-undocked";
                outputs = [
                  { criteria = "AU Optronics 0x2036 Unknown"; position = "0,0"; mode = "2560x1440"; scale = 1.0; }
                ];
              };
            }
            {
              profile = {
                name = "home-docked";
                outputs = [
                  { criteria = "AU Optronics 0x2036 Unknown"; position = "0,0"; mode = "2560x1440"; scale = 1.0; }
                  { criteria = "Dell Inc. DELL U2718Q FN84K01T095L"; position = "2560,0"; mode = "3840x2160"; scale = 1.0; }
                ];
              };
            }
          ];
        };
        mako = {
          enable = true;
          settings.font = "Hack Regular 9";
        };
        playerctld.enable = true;
        swayidle = {
          enable = true;
          systemdTarget = "river-session.target";
          events = [{ event = "before-sleep"; command = "${lock}"; }];
          timeouts = [
            { timeout = 150; command = "${pkgs.wlopm}/bin/wlopm --off '*'"; resumeCommand = "${pkgs.wlopm}/bin/wlopm --on '*'"; }
            { timeout = 300; command = "${lock}"; }
          ];
        };
      };
      systemd.user.targets = {
        river-session.Unit = {
          Description = "river compositor session";
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
        "river/init" = {
          source = river-init;
          onChange = ''
            if [ -d /run/user/$UID ]
            then
                WAYLAND_DISPLAY="$(${pkgs.findutils}/bin/find /run/user/$UID -mindepth 1 -maxdepth 1 -type s -name wayland-\*)"
                if [ -S "WAYLAND_DISPLAY" ]
                then
                ${river-init}
                fi
            fi
          '';
        };
        "waybar/frappe.css".text = ''
          @define-color base   #303446;
          @define-color mantle #292c3c;
          @define-color crust  #232634;

          @define-color text     #c6d0f5;
          @define-color subtext0 #a5adce;
          @define-color subtext1 #b5bfe2;

          @define-color surface0 #414559;
          @define-color surface1 #51576d;
          @define-color surface2 #626880;

          @define-color overlay0 #737994;
          @define-color overlay1 #838ba7;
          @define-color overlay2 #949cbb;

          @define-color blue      #8caaee;
          @define-color lavender  #babbf1;
          @define-color sapphire  #85c1dc;
          @define-color sky       #99d1db;
          @define-color teal      #81c8be;
          @define-color green     #a6d189;
          @define-color yellow    #e5c890;
          @define-color peach     #ef9f76;
          @define-color maroon    #ea999c;
          @define-color red       #e78284;
          @define-color mauve     #ca9ee6;
          @define-color pink      #f4b8e4;
          @define-color flamingo  #eebebe;
          @define-color rosewater #f2d5cf;
        '';
        "waybar/latte.css".text = ''
          @define-color base   #eff1f5;
          @define-color mantle #e6e9ef;
          @define-color crust  #dce0e8;

          @define-color text     #4c4f69;
          @define-color subtext0 #6c6f85;
          @define-color subtext1 #5c5f77;

          @define-color surface0 #ccd0da;
          @define-color surface1 #bcc0cc;
          @define-color surface2 #acb0be;

          @define-color overlay0 #9ca0b0;
          @define-color overlay1 #8c8fa1;
          @define-color overlay2 #7c7f93;

          @define-color blue      #1e66f5;
          @define-color lavender  #7287fd;
          @define-color sapphire  #209fb5;
          @define-color sky       #04a5e5;
          @define-color teal      #179299;
          @define-color green     #40a02b;
          @define-color yellow    #df8e1d;
          @define-color peach     #fe640b;
          @define-color maroon    #e64553;
          @define-color red       #d20f39;
          @define-color mauve     #8839ef;
          @define-color pink      #ea76cb;
          @define-color flamingo  #dd7878;
          @define-color rosewater #dc8a78;
        '';
      };
    };
  };
}  
