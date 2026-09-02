{
  config,
  lib,
  pkgs,
  ...
}:

let
  enhanceEntry = after: before: name: entry: {
    inherit (entry) data;
    before = entry.before ++ before;
    after = entry.after ++ after;
  };
  username = config.chvp.username;
  dag = lib.hm.dag;
  mergedConfig = {
    prelude = dag.entryAnywhere {
      elisp = ''
        ;;; init --- My emacs init file
        ;;; Commentary:
        ;;; Code:
      '';
    };
    early = dag.entryAfter [ "prelude" ] {
      elisp = ''
        ;; End of early init
      '';
    };
    late = dag.entryAfter [ "early" ] {
      elisp = ''
        ;; Start of late init
      '';
    };
    postlude = dag.entryAfter [ "late" ] {
      elisp = ''
        (provide 'init)
        ;;; init.el ends here
      '';
    };
  }
  // (builtins.mapAttrs (enhanceEntry [ "prelude" ] [ "early" ]) config.chvp.base.emacs.earlyConfig)
  // (builtins.mapAttrs (enhanceEntry [ "early" ] [ "late" ]) config.chvp.base.emacs.config)
  // (builtins.mapAttrs (enhanceEntry [ "late" ] [ "postlude" ]) config.chvp.base.emacs.lateConfig);
  sortedConfig = builtins.map (el: el.data) (dag.topoSort mergedConfig).result;
  packageFunctions = builtins.map (el: el.packages or (epkgs: [ ])) sortedConfig;
  fullInit = lib.strings.join "\n" (builtins.map (el: el.elisp or "") sortedConfig);
  package = (pkgs.emacsPackagesFor pkgs.emacs-pgtk).emacsWithPackages (
    epkgs: builtins.concatLists (builtins.map (fun: fun epkgs) packageFunctions)
  );
in
{
  options.chvp.base.emacs = {
    earlyConfig = lib.mkOption {
      default = { };
    };
    config = lib.mkOption {
      default = { };
    };
    lateConfig = lib.mkOption {
      default = { };
    };
  };

  config = {
    chvp.base = {
      emacs = {
        earlyConfig = {
          no-littering = dag.entryAnywhere {
            packages = epkgs: [ epkgs.no-littering ];
            elisp = ''
              (setopt user-emacs-directory (expand-file-name "~/.cache/emacs/"))
              (require 'no-littering)
              (no-littering-theme-backups)
            '';
          };
        };
        config = {
          avy = dag.entryAfter [ "general" ] {
            packages = epkgs: [ epkgs.avy ];
            elisp = ''
              ;; Insert decision characters instead of overwriting
              (setopt avy-style 'pre)
              (require 'avy)
              (lmap
                "j" '("jump" . nil)
                "jc" '("character" . avy-goto-char)
                "j2" '("2 character sequence" . avy-goto-char-2)
                "jl" '("line" . avy-goto-line)
                "jw" '("word" . avy-goto-word-0)
                "js" '("word with character" . avy-goto-word-1)
              )
            '';
          };
          better-defaults = dag.entryAnywhere {
            packages = epkgs: [ epkgs.better-defaults ];
            elisp = ''
              ;; Better defaults that aren't defaults for some reason
              (require 'better-defaults)
              ;; But don't enable ido-mode...
              (ido-mode nil)
            '';
          };
          cape = dag.entryAnywhere {
            packages = epkgs: [ epkgs.cape ];
            elisp = builtins.readFile ./cape.el;
          };
          catppuccin-theme = dag.entryAnywhere {
            packages = epkgs: [ epkgs.catppuccin-theme ];
            elisp = builtins.readFile ./catppuccin-theme.el;
          };
          consult = dag.entryAfter [ "general" ] {
            packages = epkgs: [ epkgs.consult ];
            elisp = ''
              ;; Replacements for emacs built-ins that better integrate with `vertico'.
              (require 'consult)
              (lmap
                "bb" '("switch" . consult-buffer)
                "fr" '("recent" . consult-recent-file)
                "ha" '("apropos" . consult-apropos)
                "ss" '("search" . consult-line)
              )
            '';
          };
          corfu = dag.entryAfter [ "diminish" ] {
            packages = epkgs: [ epkgs.corfu ];
            elisp = builtins.readFile ./corfu.el;
          };
          corfu-prescient = dag.entryAfter [ "corfu" "prescient" ] {
            packages = epkgs: [ epkgs.corfu-prescient ];
            elisp = ''
              (setopt corfu-prescient-enable-filtering nil) ;; Handled by orderless
              (require 'corfu-prescient)
              (corfu-prescient-mode 1)
            '';
          };
          diminish = dag.entryAnywhere {
            packages = epkgs: [ epkgs.diminish ];
            elisp = ''
              ;; Hide minor modes
              (require 'diminish)
            '';
          };
          emacs = dag.entryAnywhere {
            elisp = builtins.readFile ./emacs.el;
          };
          flycheck = dag.entryAfter [ "diminish" ] {
            packages = epkgs: [ epkgs.flycheck ];
            elisp = builtins.readFile ./flycheck.el;
          };
          evil = dag.entryAnywhere {
            packages = epkgs: [
              epkgs.evil
              epkgs.evil-collection
            ];
            elisp = ''
              ;; Vim keybindings in emacs
              (setopt evil-want-integration t)
              (setopt evil-want-keybinding nil)
              (require 'evil)
              (evil-mode 1)
              (require 'evil-collection)
              (evil-collection-init)
            '';
          };
          general = dag.entryAfter [ "evil" ] {
            packages = epkgs: [ epkgs.general ];
            elisp = builtins.readFile ./general.el;
          };
          orderless = dag.entryAfter [ "vertico" ] {
            packages = epkgs: [ epkgs.orderless ];
            elisp = ''
              (require 'orderless)
              (setopt completion-styles '(orderless basic))
              (setopt orderless-matching-styles '(orderless-literal orderless-initialism orderless-prefixes))
            '';
          };
          org = dag.entryAfter [ "general" ] {
            packages = epkgs: [ epkgs.org ];
            elisp = builtins.readFile ./org.el;
          };
          prescient = dag.entryAnywhere {
            packages = epkgs: [ epkgs.prescient ];
            elisp = ''
              (setopt prescient-aggressive-file-save t)
              (setopt prescient-history-length 10000)
              (setopt prescient-frequency-threshold 0.00005)
              (require 'prescient)
              (prescient-persist-mode 1)
            '';
          };
          rainbow-delimiters = dag.entryAnywhere {
            packages = epkgs: [ epkgs.rainbow-delimiters ];
            elisp = ''
              (require 'rainbow-delimiters)
              (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
            '';
          };
          tempel = dag.entryAfter [ "cape" "general" ] {
            packages = epkgs: [ epkgs.tempel ];
            elisp = ''
              (setopt tempel-path "${./snippets}")
              (require 'tempel)
              (lmap
                "t"  '("template" . nil)
                "ti" '("insert" . tempel-insert)
              )
            '';
          };
          undo-fu = dag.entryBefore [ "evil" ] {
            packages = epkgs: [ epkgs.undo-fu ];
            elisp = ''
              (setopt undo-fu-ignore-keyboard-quit t) ;; Non-linear behaviour needs an explicit call
              (setopt evil-undo-system 'undo-fu) ;; Tell evil to use undo-fu
              (setopt undo-limit 67108864) ;; Store much more undo information (64MiB)
              (setopt undo-strong-limit 100663296) ;; Store much more undo information (96MiB)
              (setopt undo-outer-limit 1006632960) ;; Store much more undo information (960MiB)
              (require 'undo-fu)
            '';
          };
          undo-fu-session = dag.entryAfter [ "undo-fu" ] {
            packages = epkgs: [ epkgs.undo-fu-session ];
            elisp = ''
              (setopt undo-fu-session-compression 'zst) ;; Use zst compression for the undo files
              (setopt undo-fu-session-file-limit 10000) ;; Start removing old undo files after 10000 files
              (require 'undo-fu-session)
              (undo-fu-session-global-mode)
            '';
          };
          vertico = dag.entryAfter [ "diminish" ] {
            packages = epkgs: [ epkgs.vertico ];
            elisp = ''
              (setopt vertico-count 20)
              (require 'vertico)
              (vertico-mode)
              (diminish 'vertico-mode)
            '';
          };
          vertico-prescient = dag.entryAfter [ "vertico" "prescient" ] {
            packages = epkgs: [ epkgs.vertico-prescient ];
            elisp = ''
              (setopt vertico-prescient-enable-filtering nil)
              (require 'vertico-prescient)
              (vertico-prescient-mode 1)
            '';
          };
          vundo = dag.entryAnywhere {
            packages = epkgs: [ epkgs.vundo ];
            elisp = ''
              (require 'vundo)
            '';
          };
          which-key = dag.entryAnywhere {
            packages = epkgs: [ epkgs.which-key ];
            elisp = ''
              (require 'which-key)
              (which-key-mode)
              (diminish 'which-key-mode)
            '';
          };
        };
      };
      zfs.homeLinks = [
        {
          path = ".cache/emacs";
          type = "cache";
        }
      ];
    };
    home-manager.users.${username} = {
      home = {
        file = {
          ".emacs.d/init.el".text = fullInit;
          ".emacs.d/early-init.el".source = ./early-init.el;
        };
        packages = [
          (pkgs.writeShellScriptBin "emacs" ''${package}/bin/emacsclient -c "$@"'')
          (pkgs.writeShellScriptBin "emacsclient" ''${package}/bin/emacsclient "$@"'')
        ];
        sessionVariables = {
          EDITOR = "emacs";
        };
      };
      services.emacs = {
        enable = true;
        client.enable = true;
        socketActivation.enable = true;
        package = package;
      };
    };
  };
}
