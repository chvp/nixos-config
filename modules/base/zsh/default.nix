{ config, lib, pkgs, ... }:

let
  base = (home: {
    home.packages = [ pkgs.autojump ];
    programs.zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableSyntaxHighlighting = true;
      autocd = true;
      dotDir = ".config/zsh";
      history = {
        expireDuplicatesFirst = true;
        path = "${config.chvp.cachePrefix}${home}/.local/share/zsh/history";
      };
      oh-my-zsh = {
        enable = true;
        plugins = [
          "autojump"
          "common-aliases"
          "extract"
          "history-substring-search"
          "git"
          "systemd"
          "tmux"
        ];
        theme = "robbyrussell";
      };
      sessionVariables = { DEFAULT_USER = "charlotte"; };
    };
  });
in
{
  chvp.base.zfs.systemLinks = [{ path = "/root/.local/share/autojump"; type = "cache"; }];
  chvp.base.zfs.homeLinks = [{ path = ".local/share/autojump"; type = "cache"; }];
  home-manager.users.charlotte = { ... }: (base "/home/charlotte");
  home-manager.users.root = { ... }: (base "/root");
}
