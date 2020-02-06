{ pkgs, ... }:

{
  home-manager.users.charlotte = { pkgs, ... }: {
    xdg.configFile = {
      "waybar/config".text = ''
        {
          "layer": "bottom",
          "position": "top",
          "modules-left": ["sway/workspaces", "sway/mode"],
          "modules-center": ["sway/window"],
          "modules-right": ["idle_inhibitor", "pulseaudio", "network", "backlight", "battery", "tray", "clock"],
          "sway/mode": {
            "format": "<span style=\"italic\">{}</span>"
          },
          "sway/window": {
            "max-length": 50
          },
          "idle_inhibitor": {
            "format": "{icon}",
            "format-icons": {
              "activated": "",
              "deactivated": ""
            }
          },
          "tray": {
            "spacing": 10
          },
          "clock": {
            "format": "{:%a %Y-%m-%d %H:%M}"
          },
          "cpu": {
            "format": "{usage}% "
          },
          "memory": {
            "format": "{}% "
          },
          "backlight": {
            "format": "{percent}% {icon}",
            "format-icons": ["", ""]
          },
          "battery": {
            "bat": "BAT0",
            "states": {
              "warning": 30,
              "critical": 15
            },
            "format": "{capacity}% {icon}",
            "format-icons": ["", "", "", "", ""]
          },
          "network": {
            "format-wifi": "{essid} ({signalStrength}%) ",
            "format-ethernet": "{ipaddr} ",
            "format-disconnected": "Disconnected ⚠"
          },
          "pulseaudio": {
            "format": "{volume}% {icon}",
            "format-bluetooth": "{volume}% {icon}",
            "format-muted": "",
            "format-icons": {
              "headphones": "",
              "handsfree": "",
              "headset": "",
              "phone": "",
              "portable": "",
              "car": "",
              "default": ["", ""]
            },
            "on-click": "pavucontrol"
          }
        }
      '';
      "waybar/style.css".text = ''
        * {
          border: none;
          border-radius: 0;
          font-family: Noto;
          font-size: 13px;
          min-height: 0;
        }

        window#waybar {
          background: rgba(43, 48, 59, 0.5);
          border-bottom: 3px solid rgba(100, 114, 125, 0.5);
          color: white;
        }

        /* https://github.com/Alexays/Waybar/wiki/FAQ#the-workspace-buttons-have-a-strange-hover-effect */
        #workspaces button {
          padding: 0 5px;
          background: transparent;
          color: white;
          border-bottom: 3px solid transparent;
        }

        #workspaces button.focused {
          background: #64727D;
          border-bottom: 3px solid white;
        }

        #mode {
          background: #64727D;
          border-bottom: 3px solid white;
        }

        #clock, #battery, #cpu, #memory, #backlight, #network, #pulseaudio, #custom-spotify, #tray, #mode, #idle_inhibitor {
          padding: 5px 10px;
          margin: 0 5px;
        }

        #clock {
          background-color: #64727D;
        }

        #battery {
          background-color: #ffffff;
          color: black;
        }

        #battery.charging {
          color: white;
          background-color: #26A65B;
        }

        @keyframes blink {
          to {
            background-color: #ffffff;
            color: black;
          }
        }

        #battery.critical:not(.charging) {
          background: #f53c3c;
          color: white;
          animation-name: blink;
          animation-duration: 0.5s;
          animation-timing-function: linear;
          animation-iteration-count: infinite;
          animation-direction: alternate;
        }

        #cpu {
          background: #2ecc71;
          color: #000000;
        }

        #memory {
          background: #9b59b6;
        }

        #backlight {
          background: #90b1b1;
        }

        #network {
          background: #2980b9;
        }

        #network.disconnected {
          background: #f53c3c;
        }

        #pulseaudio {
          background: #f1c40f;
          color: black;
        }

        #pulseaudio.muted {
          background: #90b1b1;
          color: #2a5c45;
        }

        #tray {
          background-color: #2980b9;
        }

        #idle_inhibitor {
          background-color: #2d3436;
        }

        #idle_inhibitor.activated {
          background-color: #ecf0f1;
          color: #2d3436;
        }
      '';
    };
  };
}
