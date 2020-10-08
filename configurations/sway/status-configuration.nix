{ pkgs, ... }:
let
  mic-status = pkgs.writeScript "mic-status" ''
    #!${pkgs.zsh}/bin/zsh

    if [ "$(${pkgs.pulseaudio}/bin/pactl list sources | grep -o 'Mute: yes')" = "Mute: yes" ]
    then
      echo -e '\uf131'
    else
      echo -e '\uf130'
    fi
  '';
  mail-status = pkgs.writeScript "mail-status" ''
    #!${pkgs.zsh}/bin/zsh

    mails=$(${pkgs.mblaze}/bin/mlist -N ~/mail/*/INBOX | wc -l)
    if [ "$mails" -gt 0 ]
    then
      echo "{ \"state\": \"Info\", \"text\": \" 📬 $mails\" }"
    else
      echo "{ \"state\": \"Idle\", \"text\": \" 📭 $mails\" }"
    fi
  '';
in
pkgs.writeText "configuration.toml" ''
  [theme]
  name = "gruvbox-light"
  [theme.overrides]
  idle_bg="#fbffff"
  idle_fg="#535c65"
  info_bg="#2b7ab2"
  info_fg="#fbffff"
  good_bg="#4d7f43"
  good_fg="#fbffff"
  warning_bg="#906c33"
  warning_fg="#fbffff"
  critical_bg="#ae5865"
  critical_fg="#fbffff"
  separator=""

  [icons]
  name = "awesome"

  [[block]]
  block = "net"
  device = "wlp2s0"
  ssid = true
  signal_strength = true
  speed_up = false
  speed_down = false
  hide_missing = true
  hide_inactive = true

  [[block]]
  block = "net"
  device = "wlp0s20f3"
  ssid = true
  signal_strength = false
  speed_up = false
  speed_down = false
  hide_missing = true
  hide_inactive = true

  [[block]]
  block = "net"
  device = "enp0s31f6"
  ip = true
  speed_up = false
  speed_down = false
  hide_missing = true
  hide_inactive = true

  [[block]]
  block = "net"
  device = "enp0s20f0u1u2"
  ip = true
  speed_up = false
  speed_down = false
  hide_missing = true
  hide_inactive = true

  [[block]]
  block = "battery"

  [[block]]
  block = "backlight"

  [[block]]
  block = "sound"

  [[block]]
  block = "custom"
  command = "${mic-status}"
  interval = 1
  on_click = "${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle"

  [[block]]
  block = "custom"
  json = true
  command = "${mail-status}"
  interval = 1
  on_click = "${pkgs.offlineimap}/bin/offlineimap"

  [[block]]
  block = "time"
  interval = 1
  format = "%a %d/%m %H:%M:%S"
''
