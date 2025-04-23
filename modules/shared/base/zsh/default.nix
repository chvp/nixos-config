{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
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
      initContent = ''
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
      shellAliases = {
        gupd = "gfa && gprom";
      };
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
  options.chvp.base.zsh.usersToConfigure = lib.mkOption {
    default = [ username ];
  };

  config = {
    programs.zsh.enable = true;
    chvp.base.zfs.homeLinks = lib.mkIf (builtins.elem username config.chvp.base.zsh.usersToConfigure) [{ path = ".local/share/autojump"; type = "cache"; }];
    chvp.base.zfs.systemLinks = lib.mkIf (builtins.elem "root" config.chvp.base.zsh.usersToConfigure) [{ path = "/root/.local/share/autojump"; type = "cache"; }];
  } // {
    home-manager.users = builtins.foldl' (a: b: a // b) { } (
      builtins.map
        (name: { "${name}" = { ... }: (base config.users.users.${name}.home); })
        config.chvp.base.zsh.usersToConfigure
    );
  };
}
