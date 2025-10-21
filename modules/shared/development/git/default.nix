{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
  homeDir = config.home-manager.users.${username}.home.homeDirectory;
  sshKeyFile = "${config.chvp.dataPrefix}${homeDir}/.ssh/id_ed25519";
in
{
  options.chvp.development.git = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
    email = lib.mkOption {
      type = lib.types.str;
      default = "charlotte@vanpetegem.be";
      example = "charlotte@vanpetegem.be";
      description = ''
        Default email set in global git config.
      '';
    };
  };

  config = lib.mkIf config.chvp.development.git.enable {
    chvp.base.emacs.extraConfig = [
      ''
        ;; Magit GitHub/GitLab integration
        (use-package forge
          :after magit)

        ;; Git integration
        (use-package magit
          :init
          (setq forge-add-default-bindings nil)
          :general
          (lmap
            "g" '(:ignore t :which-key "git")
            "gs" '(magit-status :which-key "status")
          )
        )

        ;; Project management
        (use-package project
          :custom
          (project-switch-commands
            '(
              (project-find-file "find file")
              (consult-ripgrep "find regexp" ?r)
              (project-eshell "eshell")
            )
            "Change default actions when switching project"
          )
          :general
          (lmap
            "p"  '(:ignore t :which-key "project")
            "pf" '(project-find-file :which-key "find")
            "pp" '(project-switch-project :which-key "switch")
            "pr" '(project-query-replace-regexp :which-key "replace")
            "ps" '(consult-ripgrep :search "incsearch")
            "pS" '(project-find-regexp :which-key "search")
            "p!" '(project-shell-command :which-key "command")
            "p&" '(project-async-shell-command :which-key "task")
          )
        )
      ''
    ];
    home-manager.users.${username} = {
      programs.git = {
        enable = true;
        lfs.enable = true;
        signing = {
          format = "ssh";
          key = sshKeyFile;
          signByDefault = true;
        };
        settings = {
          branch.autoSetupRebase = "always";
          fetch.prune = true;
          github.user = "chvp";
          merge.conflictStyle = "diff3";
          pull.rebase = true;
          push.autoSetupRemote = true;
          rebase.autoStash = true;
          rerere.enabled = true;
          user = {
            email = config.chvp.development.git.email;
            name = "Charlotte Van Petegem";
          };
        };
        ignores = [
          ".DS_Store"
          ".data"
          ".direnv"
          ".envrc"
          ".idea"
          ".dir-locals.el"
        ];
      };
    };
  };
}
