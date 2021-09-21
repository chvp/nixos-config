{ config, lib, ... }:

{
  imports = [
    ./android
    ./docker
    ./git
  ];

  options.chvp.development.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.development.enable {
    chvp = {
      base.emacs.extraConfig = [
        ''
          ;; Editorconfig
          (use-package editorconfig
            :diminish (editorconfig-mode)
            :custom (editorconfig-get-properties-function 'editorconfig-get-properties)
            :config (editorconfig-mode 1)
            )
          
          ;; Language server support
          (use-package lsp-mode
            :commands (lsp lsp-deferred)
            :config (lsp-enable-which-key-integration t)
            :general
            (lmap lsp-mode-map
              "SPC" '(:keymap lsp-command-map)
              )
            )
          
          ;; Markdown syntax support
          (use-package markdown-mode
            :commands (markdown-mode gfm-mode)
            :mode ("README\\.md\\'" . gfm-mode)
            :mode ("\\.md\\'" . markdown-mode)
            :mode ("\\.markdown\\'" . markdown-mode)
            )
          
          ;; YAML syntax support
          (use-package yaml-mode
            :mode "\\.yml\\'"
            :mode "\\.yaml\\'"
            )

          ;; R language support
          (use-package ess
            :mode "\\.r\\'"
            :mode "\\.R\\'"
            )

          ;; Haskell language support
          (use-package haskell-mode
            :mode "\\.hs\\'"
            )

          ;; Python syntax support
          (use-package python-mode
            :mode "\\.py\\'"
            )
          
          ;; Ruby language support
          (use-package ruby-mode
           :ensure nil ;; Included with emacs
           :mode "\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'"
           :mode "\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'"
           :custom
           (ruby-insert-encoding-magic-comment nil "Don't insert encoding magic comment")
           )

          ;; Rust language support
          (use-package rust-mode
            :mode "\\.rs\\'"
            )

          ;; TypeScript language support
          (use-package typescript-mode
            :mode "\\.ts\\'"
            )

          ;; Vue language support
          (use-package vue-mode
            :mode "\\.vue\\'"
            )
          
          ;; HTML (and HTML template) support
          (use-package web-mode
            :mode "\\.html\\'"
            :mode "\\.html\\.erb\\'"
            )
        ''
      ];
      development = {
        docker.enable = lib.mkDefault true;
        git.enable = lib.mkDefault true;
      };
    };

    boot.kernel.sysctl."fs.inotify.max_user_watches" = 524288;
  };
}
