local wezterm = require 'wezterm';

return {
  font = wezterm.font("Fira Code"),
  font_size = 9.0,
  colors = {
    foreground = "#000000",
    background = "#ffffff",
    cursor_bg = "#777777",
    cursor_fg = "#ffffff",
    selection_bg = "#000000",
    selection_fg = "#ffffff",
    ansi = { "#282828", "#a60000", "#005e00", "#813e00", "#0031a9", "#721045", "#00538b", "#f8f8f8" },
    brights = { "#000000", "#972500", "#315b00", "#70480f", "#2544bb", "#8f0075", "#30517f", "#ffffff" },
  },
  enable_scroll_bar = false,
  audible_bell = "Disabled",
  check_for_updates = false,
  enable_tab_bar = false,
  cursor_blink_ease_in = "Constant",
  cursor_blink_ease_out = "Constant",
  default_cursor_style = "BlinkingBlock",
  unicode_version = 14,
  visual_bell = {
    fade_in_function = "EaseIn",
    fade_in_duration_ms = 150,
    fade_out_function = "EaseOut",
    fade_out_duration_ms = 150,
  },
  window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
  },
}
