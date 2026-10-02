{
  config,
  lib,
  pkgs,
  ...
}:

let
  username = config.chvp.username;
  base = (
    home: {
      home.packages = [ pkgs.autojump ];
      programs.zsh = {
        enable = true;
        autosuggestion.enable = true;
        syntaxHighlighting.enable = true;
        autocd = true;
        dotDir = "${home}/.config/zsh";
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

          # This is an override for a function added by oh-my-zsh. This version
          # switches the lookup order; it first checks the remotes for the
          # default branch name and then looks at the local refs.
          git_main_branch () {
            command git rev-parse --git-dir &> /dev/null || return
            for remote in origin upstream
            do
              ref=$(command git rev-parse --abbrev-ref $remote/HEAD 2>/dev/null) 
              if [[ $ref == $remote/* ]]
              then
                echo ''${ref#"$remote/"}
                return 0
              fi
            done
            for ref in refs/{heads,remotes/{origin,upstream}}/{main,trunk,mainline,default,stable,master}
            do
              if command git show-ref -q --verify $ref
              then
                echo ''${ref:t}
                return 0
              fi
            done
            echo master
            return 1
          }

          alias grbmb='git rebase $(git merge-base HEAD $(git_main_branch))'
          alias grbmba='git rebase $(git merge-base HEAD $(git_main_branch)) --autosquash'
          alias grbmbi='git rebase $(git merge-base HEAD $(git_main_branch)) --interactive'
          alias grbmbia='git rebase $(git merge-base HEAD $(git_main_branch)) --interactive --autosquash'
        ''
        + (lib.optionalString
          (home == config.users.users.${username}.home && config.chvp.graphical.compositor.enable)
          ''
            if [ "$(darkman get)" = "dark" ]
            then
              pkill -SIGUSR1 foot
            fi
          ''
        );
        shellAliases = {
          gupd = "gfa && gprom";
        };
        sessionVariables = {
          DEFAULT_USER = username;
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
      };
    }
  );
in
{
  options.chvp.base.zsh.usersToConfigure = lib.mkOption {
    default = [
      username
      "root"
    ];
  };

  config = {
    programs.zsh.enable = true;
    chvp.base.zfs.homeLinks = lib.mkIf (builtins.elem username config.chvp.base.zsh.usersToConfigure) [
      {
        path = ".local/share/autojump";
        type = "cache";
      }
    ];
    chvp.base.zfs.systemLinks = lib.mkIf (builtins.elem "root" config.chvp.base.zsh.usersToConfigure) [
      {
        path = "/root/.local/share/autojump";
        type = "cache";
      }
    ];
  }
  // {
    home-manager.users = builtins.foldl' (a: b: a // b) { } (
      builtins.map (name: {
        "${name}" = (base config.users.users.${name}.home);
      }) config.chvp.base.zsh.usersToConfigure
    );
  };
}
