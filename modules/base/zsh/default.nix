{ config, lib, pkgs, ... }:

let
  base = (home: {
    home.packages = [ pkgs.autojump ];
    programs.zsh = {
      enable = true;
      enableAutosuggestions = true;
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
      plugins = [{
        name = "zsh-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "0.7.1";
          sha256 = "03r6hpb5fy4yaakqm3lbf4xcvd408r44jgpv4lnzl9asp4sb9qc0";
        };
      }];
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
