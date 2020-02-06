{ ... }:

{
  home-manager.users.charlotte = { ... }: {
    programs.tmux = {
      enable = true;
      clock24 = true;
      extraConfig = ''
          bind q kill-session
          bind v run-shell "tmux setw main-pane-width $(($(tmux display -p '#{window_width}') * 70 / 100)); tmux select-layout main-vertical"
          bind h run-shell "tmux setw main-pane-height $(($(tmux display -p '#{window_height}') * 70 / 100)); tmux select-layout main-horizontal"
      '';
      keyMode = "vi";
      tmuxinator.enable = true;
    };
  };
}
