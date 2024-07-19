{ config, lib, ... }:

let
  username = config.chvp.username;
  base = {
    programs.tmux = {
      enable = true;
      clock24 = true;
      extraConfig = ''
        bind q kill-session
        bind v run-shell "tmux setw main-pane-width $(($(tmux display -p '#{window_width}') * 70 / 100)); tmux select-layout main-vertical"
        bind h run-shell "tmux setw main-pane-height $(($(tmux display -p '#{window_height}') * 70 / 100)); tmux select-layout main-horizontal"

        set -g default-terminal "screen-256color"
        set -sg escape-time 10
      '';
      keyMode = "vi";
    };
  };
in
{
  options.chvp.base.tmux.usersToConfigure = lib.mkOption {
    default = [ username ];
  };

  config.home-manager.users = builtins.foldl' (a: b: a // b) { } (
    builtins.map (name: { "${name}" = base; }) config.chvp.base.tmux.usersToConfigure
  );
}
