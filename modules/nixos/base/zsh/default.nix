{ config, lib, pkgs, ... }:

let
  base = (home: {
    home.packages = [ pkgs.autojump ];
    programs.zsh = {
      enable = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
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

        lightmode() {
          printf "\033]10;rgb:4c/4f/69\007"
          printf "\033]11;rgb:ef/f1/f5\007"
          printf "\033]17;rgb:cc/d0/da\007"
          printf "\033]19;rgb:4c/4f/69\007"
          printf "\033]4;0;rgb:5c/5f/77\007"
          printf "\033]4;1;rgb:d2/0f/39\007"
          printf "\033]4;2;rgb:40/a0/2b\007"
          printf "\033]4;3;rgb:df/8e/1d\007"
          printf "\033]4;4;rgb:1e/66/f5\007"
          printf "\033]4;5;rgb:ea/76/cb\007"
          printf "\033]4;6;rgb:17/92/99\007"
          printf "\033]4;7;rgb:ac/b0/be\007"
          printf "\033]4;8;rgb:6c/6f/85\007"
          printf "\033]4;9;rgb:d2/0f/39\007"
          printf "\033]4;10;rgb:40/a0/2b\007"
          printf "\033]4;11;rgb:df/8e/1d\007"
          printf "\033]4;12;rgb:1e/66/f5\007"
          printf "\033]4;13;rgb:ea/76/cb\007"
          printf "\033]4;14;rgb:17/92/99\007"
          printf "\033]4;15;rgb:bc/c0/cc\007"
        }

        darkmode() {
          printf "\033]10;rgb:c6/d0/f5\007"
          printf "\033]11;rgb:30/34/46\007"
          printf "\033]17;rgb:41/45/59\007"
          printf "\033]19;rgb:c6/d0/f5\007"
          printf "\033]4;0;rgb:51/57/6d\007"
          printf "\033]4;1;rgb:e7/82/84\007"
          printf "\033]4;2;rgb:a6/d1/89\007"
          printf "\033]4;3;rgb:e5/c8/90\007"
          printf "\033]4;4;rgb:8c/aa/ee\007"
          printf "\033]4;5;rgb:f4/b8/e4\007"
          printf "\033]4;6;rgb:81/c8/be\007"
          printf "\033]4;7;rgb:b5/bf/e2\007"
          printf "\033]4;8;rgb:62/68/80\007"
          printf "\033]4;9;rgb:e7/82/84\007"
          printf "\033]4;10;rgb:a6/d1/89\007"
          printf "\033]4;11;rgb:e5/c8/90\007"
          printf "\033]4;12;rgb:8c/aa/ee\007"
          printf "\033]4;13;rgb:f4/b8/e4\007"
          printf "\033]4;14;rgb:81/c8/be\007"
          printf "\033]4;15;rgb:a5/ad/ce\007"
        }

        TRAPUSR1() {
          lightmode
        }

        TRAPUSR2() {
          darkmode
        }

        if type darkman >/dev/null
        then
          if [ "$(darkman get)" = "dark" ]
          then
            darkmode
          else
            lightmode
          fi
        fi
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
  programs.zsh.enable = true;
}
