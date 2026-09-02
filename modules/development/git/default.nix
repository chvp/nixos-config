{
  config,
  lib,
  pkgs,
  ...
}:

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
    chvp.base.emacs.config = {
      forge = lib.hm.dag.entryAfter [ "magit" ] {
        packages = epkgs: [ epkgs.forge ];
        elisp = ''
          (require 'forge)
        '';
      };
      magit = lib.hm.dag.entryAfter [ "general" ] {
        packages = epkgs: [ epkgs.magit ];
        elisp = ''
          (setq forge-add-default-bindings nil)
          (require 'magit)
          (lmap
            "g"  '("git" . nil)
            "gs" '("status". magit-status)
          )
        '';
      };
      project = lib.hm.dag.entryAfter [ "general" "consult" ] {
        packages = epkgs: [ epkgs.project ];
        elisp = ''
          (setopt project-vc-merge-submodules nil)
          (setopt project-switch-commands
                  '(
                    (project-find-file "find file")
                    (consult-ripgrep "find regexp" ?r)
                    (project-eshell "eshell")))
          (require 'project)
          (lmap
            "p"  '("project" . nil)
            "pf" '("find" . project-find-file)
            "pp" '("switch" . project-switch-project)
            "pr" '("replace" . project-query-replace-regexp)
            "ps" '(consult-ripgrep :search "incsearch")
            "pS" '("search" . project-find-regexp)
            "p!" '("command" . project-shell-command)
            "p&" '("task" . project-async-shell-command)
          )
        '';
      };
    };
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
          init.defaultBranch = "main";
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
