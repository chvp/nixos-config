{ config, lib, pkgs, ... }:

{
  options.chvp.development.git = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
    email = lib.mkOption {
      type = lib.types.str;
      default = "charlotte@vanpetegem.me";
      example = "charlotte@vanpetegem.me";
      description = ''
        Default email set in global git config.
      '';
    };
  };

  config =
    let
      base = {
        home.packages = with pkgs; [
          gitAndTools.gitflow
          git-crypt
        ];
        programs.git = {
          enable = true;
          extraConfig = {
            branch.autosetuprebase = "always";
            pull.rebase = true;
            github.user = "chvp";
            tag.gpgSign = true;
          };
          ignores = [
            ".data"
            ".direnv"
            ".envrc"
            ".dir-locals.el"
          ];
          signing = {
            key = "charlotte@vanpetegem.me";
            signByDefault = config.chvp.graphical.enable;
          };
          userEmail = config.chvp.development.git.email;
          userName = "Charlotte Van Petegem";
        };
      };
    in
    lib.mkIf config.chvp.development.git.enable {
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
      home-manager.users.charlotte = { ... }: base;
      home-manager.users.root = { ... }: base;
    };
}
