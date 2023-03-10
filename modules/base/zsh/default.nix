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
      initExtra = ''
        nshell() {
         local -a drvs
         for attr in "$@"; do
           drvs+=(nixpkgs#$attr)
         done
         local paths="$(nix build --no-link --print-out-paths $drvs)"
         for p in $paths; do
           export PATH="$p/bin:$PATH"
         done
        }

        nrun() {
          local drv="$1"
          shift 1
          nix run nixpkgs#$drv $@
        }

        nsrun() {
          local drv="$1"
          shift 1
          nix shell nixpkgs#$drv -c $@
        }
      '';
      sessionVariables = { DEFAULT_USER = "charlotte"; };
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
    };
  });
in
{
  chvp.base.zfs.systemLinks = [{ path = "/root/.local/share/autojump"; type = "cache"; }];
  chvp.base.zfs.homeLinks = [{ path = ".local/share/autojump"; type = "cache"; }];
  home-manager.users.charlotte = { ... }: (base "/home/charlotte");
  home-manager.users.root = { ... }: (base "/root");
}
