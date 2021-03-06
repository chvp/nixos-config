{ config, pkgs, ... }:
let
  mic-status = pkgs.writeShellScript "mic-status" ''
    if [ "$(${pkgs.pulseaudio}/bin/pactl list sources | grep -o 'Mute: yes')" = "Mute: yes" ]
    then
      echo -e '\uf131'
    else
      echo -e '\uf130'
    fi
  '';
  mail-status = pkgs.writeShellScript "mail-status" ''
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
  idle_bg="#ffffff"
  idle_fg="#000000"
  info_bg="#6aaeff"
  info_fg="#000000"
  good_bg="#5ada88"
  good_fg="#000000"
  warning_bg="#f5df23"
  warning_fg="#000000"
  critical_bg="#ff8892"
  critical_fg="#000000"
  separator=""

  [icons]
  name = "awesome"

  [[block]]
  block = "net"
  device = "wlp2s0"
  format = "{ssid}"
  hide_missing = true
  hide_inactive = true

  [[block]]
  block = "net"
  device = "wlp0s20f3"
  format = "{ssid}"
  hide_missing = true
  hide_inactive = true

  [[block]]
  block = "net"
  device = "enp0s31f6"
  format = "{ip}"
  hide_missing = true
  hide_inactive = true

  [[block]]
  block = "net"
  device = "enp0s20f0u1u2"
  format = "{ip}"
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
  on_click = "${pkgs.isync}/bin/mbsync -a && ${config.chvp.base.emacs.package}/bin/emacsclient --eval \"(mu4e-update-index)\""

  [[block]]
  block = "time"
  interval = 1
  format = "%a %d/%m %H:%M"
''
