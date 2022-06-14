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
            :general
            (lmap
              "g" '(:ignore t :which-key "git")
              "gs" '(magit-status :which-key "status")
              )
            )

          ;; Project management
          (use-package projectile
            :after consult
            :commands (projectile-project-root)
            :custom (consult-project-function #'projectile-project-root "Use projectile to determine project roots.")
            :diminish (projectile-mode)
            :config (projectile-mode 1)
            :general
            (lmap
              "p"  '(:ignore t :which-key "project")
              "pf" '(projectile-find-file :which-key "find")
              "pp" '(projectile-switch-project :which-key "switch")
              "pr" '(projectile-replace :which-key "replace")
              "ps" '(consult-ripgrep :search "incsearch")
              "pS" '(projectile-ripgrep :which-key "search")
              "p!" '(projectile-run-shell-command-in-root :which-key "command")
              "p&" '(projectile-run-async-shell-command-in-root :which-key "task")
              )
            )

          ;; Ripgrep support (needed for `projectile-ripgrep')
          (use-package ripgrep
            :after (projectile)
            )
        ''
      ];
      home-manager.users.charlotte = { ... }: base;
      home-manager.users.root = { ... }: base;
    };
}
