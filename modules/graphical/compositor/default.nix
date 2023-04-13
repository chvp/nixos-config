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
  baseWrapper = pkgs.writeShellScriptBin "river" ''
    export XDG_SESSION_TYPE=wayland
    export XDG_CURRENT_DESKTOP=river
    export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
    export QT_AUTO_SCREEN_SCALE_FACTOR=0
    export QT_SCALE_FACTOR=1
    export GDK_SCALE=1
    export GDK_DPI_SCALE=1
    export MOZ_ENABLE_WAYLAND=1
    export XCURSOR_SIZE=24
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
    riverctl map normal Super Return spawn footclient
    riverctl map normal Super D spawn 'footclient --app-id launcher -- ${launcher}/bin/launcher'

    riverctl map normal Super C spawn ${pkgs.swaylock}/bin/swaylock

    riverctl map normal Super+Shift C close

    riverctl map normal Super+Shift E exit

    riverctl map normal Super J focus-view next
    riverctl map normal Super K focus-view previous

    riverctl map normal Super+Shift J swap next
    riverctl map normal Super+Shift K swap previous

    riverctl map normal Super H focus-output next
    riverctl map normal Super L focus-output previous

    riverctl map normal Super+Shift H send-to-output next
    riverctl map normal Super+Shift L send-to-output previous

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

    riverctl float-filter-add app-id launcher
    riverctl float-filter-add app-id be.ugent.dominion.Main
    riverctl float-filter-add title "Quick Format Citation"

    riverctl default-layout rivertile
    rivertile -view-padding 0 -outer-padding 0 &

    riverctl attach-mode bottom
    riverctl background-color 0x000000
    riverctl border-color-focused 0x6aaeff
    riverctl border-color-unfocused 0xf2eff3
    riverctl border-color-urgent 0xff8892
    riverctl border-width 1
    riverctl focus-follows-cursor normal
    riverctl hide-cursor when-typing enabled
    riverctl set-cursor-warp on-output-change
    riverctl xcursor-theme Vanilla-DMZ 24

    riverctl keyboard-layout -variant altgr-intl -options compose:caps us

    configure_touchpads() {
      riverctl list-inputs | grep 'type: pointer' -B 1 | grep -vE 'type: pointer|^--$' | xargs -I '{}' riverctl input '{}' $@
    }

    configure_touchpads drag enabled
    configure_touchpads tap enabled
    configure_touchpads scroll-method two-finger

    ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY XDG_CURRENT_DESKTOP XDG_SESSION_TYPE XCURSOR_SIZE
    systemctl --user start graphical-session.target
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
    };
    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = [
        river
        color-picker
        screenshot
        pkgs.wf-recorder
        pkgs.wl-clipboard
      ];
      programs.waybar = {
        enable = true;
        settings = {
          mainBar = {
            spacing = 2;
            modules-left = [ "river/tags" ];
            modules-center = [ "river/window" ];
            modules-right = [ "idle_inhibitor" "network" "battery" "backlight" "mpris" "pulseaudio" "custom/mail-status" "clock" "tray" ];
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
              format-charging = "{capacity}% \uf1e6";
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
            network = {
              format-wifi = "{essid} ";
              format-ethernet = "{ipaddr}/{cidr} ";
              tooltip-format = "{ifname} via {gwaddr} ";
              format-linked = "{ifname} (No IP) ";
              format-disconnected = "Disconnected ⚠";
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
          * {
              font-family: Hack, monospace;
              font-size: 11px;
          }

          #window, #idle_inhibitor, #network, #battery, #backlight, #mpris, #pulseaudio, #custom-mail-status, #clock, #tray {
              padding: 0 5px;
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
              background-color: #ffffff;
              color: #000000;
          }

          #backlight {
              background-color: #6aaeff;
          }

          #battery {
              background-color: #5ada88;
          }
          #battery.good {
              background-color: #6aaeff;
          }
          #battery.warning {
              background-color: #f5df23;
          }
          #battery.critical {
              background-color: #ff8892;
          }

          #clock {
              padding-right: 0px;
          }

          #custom-mail-status.has-mail {
              background-color: #6aaeff;
          }

          #idle_inhibitor.activated {
              background-color: #6aaeff;
          }

          #pulseaudio {
              background-color: #f5df23;
          }

          #tags button {
              box-shadow: inset 0 -3px transparent
              background-color: #ffffff;
              color: #000000;
          }
          #tags button.occupied {
              background-color: #f2eff3;
          }
          #tags button.focused {
              background-color: #6aaeff;
          }
          #tags button.urgent {
              background-color: #ff8892;
          }
          #tags button:hover {
              box-shadow: inset 0 -3px #000000;
          }
        '';
        systemd.enable = true;
      };
      services = {
        kanshi = {
          enable = true;
          systemdTarget = "graphical-session.target";
          profiles = {
            "home-undocked" = {
              outputs = [
                { criteria = "AU Optronics 0x2036 Unknown"; position = "0,0"; mode = "2560x1440"; scale = 1.0; }
              ];
            };
            "home-docked" = {
              outputs = [
                { criteria = "AU Optronics 0x2036 Unknown"; position = "0,0"; mode = "2560x1440"; scale = 1.0; }
                { criteria = "Dell Inc. DELL U2718Q FN84K01T095L"; position = "2560,0"; mode = "3840x2160"; scale = 1.0; }
              ];
            };
            "work-undocked" = {
              outputs = [
                { criteria = "LG Display 0x06D6 Unknown"; position = "0,0"; mode = "1920x1080"; scale = 1.0; }
              ];
            };
            "work-docked" = {
              outputs = [
                { criteria = "LG Display 0x06D6 Unknown"; position = "0,0"; mode = "1920x1080"; scale = 1.0; }
                { criteria = "Dell Inc. DELL U2718Q FN84K83Q1KHL"; position = "1920,0"; mode = "3840x2160"; scale = 1.0; }
              ];
            };
          };
        };
        mako = {
          enable = true;
          font = "Hack Regular 9";
        };
        swayidle = {
          enable = true;
          systemdTarget = "graphical-session.target";
          events = [{ event = "before-sleep"; command = "${pkgs.swaylock}/bin/swaylock"; }];
          timeouts = [
            { timeout = 150; command = "${pkgs.wlopm}/bin/wlopm --off '*'"; resumeCommand = "${pkgs.wlopm}/bin/wlopm --on '*'"; }
            { timeout = 300; command = "${pkgs.swaylock}/bin/swaylock -fF"; }
          ];
        };
      };
      xdg.configFile."river/init" = {
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
    };
  };
}
