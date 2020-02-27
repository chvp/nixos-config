{ pkgs, ... }:

pkgs.writeText "configuration.toml" ''
  [theme]
  name = "gruvbox-light"
  [theme.overrides]
  idle_bg="#fbffff"
  idle_fg="#535c65"
  info_bg="#2b7ab2"
  info_fg="#fbffff"
  good_bg="#2b7ab2"
  good_fg="#fbffff"
  warning_bg="#2b7ab2"
  warning_fg="#fbffff"
  critical_bg="#2b7ab2"
  critical_fg="#fbffff"
  separator=""

  [icons]
  name = "awesome"

  [[block]]
  block = "focused_window"
  max_width = 100

  [[block]]
  block = "battery"

  [[block]]
  block = "backlight"

  [[block]]
  block = "sound"

  [[block]]
  block = "time"
  interval = 1
  format = "%a %d/%m %H:%M:%S"
''
