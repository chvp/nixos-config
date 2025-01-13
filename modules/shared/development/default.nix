{ config, lib, pkgs, ... }:

{
  imports = [
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
            :config
            (editorconfig-mode 1)
            )

          ;; R syntax support
          (use-package ess
            :init
            (load "ess-autoloads")
            :mode ("\\.r\\'" . ess-r-mode)
            :mode ("\\.R\\'" . ess-r-mode)
            )
          
          ;; Language server support
          (use-package eglot
            :demand t
            :general
            (lmap
              :keymaps '(prog-mode-map vue-mode-map)
              "SPC s" '(eglot :which-key "Add buffer to eglot")
              "SPC f" '(eglot-format :which-key "Format region")
              "SPC F" '(eglot-format :which-key "Format buffer")
              "SPC r" '(eglot-rename :which-key "Rename symbol")
              "SPC a" '(eglot-code-actions :which-key "Relevant local actions")
              "SPC n" '(flymake-goto-next-error :which-key "Next error")
              "SPC p" '(flymake-goto-prev-error :which-key "Previous error")
              )
            :hook (eglot-managed-mode . chvp--eglot-capf)
            :config
            (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
            (defun chvp--eglot-capf ()
              (setq-local completion-at-point-functions
                          (list (cape-super-capf
                                 #'eglot-completion-at-point
                                 #'tempel-complete)
                                #'cape-file
                                #'dabbrev-capf
                                #'cape-line)))
            )

          ;; Forth syntax support
          (use-package forth-mode
            :mode ("\\.fs\\'" . forth-mode)
            :mode ("\\.fb\\'" . forth-block-mode)
          )
          
          ;; Markdown syntax support
          (use-package markdown-mode
            :commands (markdown-mode gfm-mode)
            :mode ("README\\.md\\'" . gfm-mode)
            :mode ("\\.md\\'" . markdown-mode)
            :mode ("\\.markdown\\'" . markdown-mode)
            )

          ;; Haskell language support
          (use-package haskell-mode
            :mode "\\.hs\\'"
            :config
            (require 'haskell-doc)
            )

          ;; Folding
          (use-package origami
            :hook (prog-mode . origami-mode)
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
           :config
           (add-to-list 'eglot-server-programs `(ruby-mode . ("ruby-lsp")))
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
            :custom
            (mmm-submode-decoration-level 0 "Don't color submodes differently")
            :config
            (defun vue-eglot-init-options ()
              (let ((tsdk-path "${pkgs.typescript}/lib/node_modules/typescript/lib"))
                `(:typescript (:tsdk ,tsdk-path
                              :languageFeatures (:completion
                                                 (:defaultTagNameCase "kebab"
                                                  :defaultAttrNameCase "kebab"
                                                  :getDocumentNameCasesRequest nil
                                                  :getDocumentSelectionRequest nil)
                                                 :diagnostics
                                                 (:getDocumentVersionRequest nil))
                              :documentFeatures (:documentFormatting
                                                 (:defaultPrintWidth 100
                                                  :getDocumentPrintWidthRequest nil)
                                                 :documentSymbol t
                                                 :documentColor t)))))
            (add-to-list 'eglot-server-programs `(vue-mode . ("${pkgs.vue-language-server}/bin/vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))
            (setq vue--front-tag-lang-regex
              (concat "<%s"                               ; The tag name
                      "\\(?:"                             ; Zero of more of...
                      "\\(?:\\s-+\\w+=[\"'].*?[\"']\\)"   ; Any optional key-value pairs like type="foo/bar"
                      "\\|\\(?:\\s-+scoped\\)"            ; The optional "scoped" attribute
                      "\\|\\(?:\\s-+module\\)"            ; The optional "module" attribute
                      "\\|\\(?:\\s-+setup\\)"             ; The optional "setup" attribute
                      "\\)*"
                      "\\(?:\\s-+lang=[\"']%s[\"']\\)"    ; The language specifier (required)
                      "\\(?:"                             ; Zero of more of...
                      "\\(?:\\s-+\\w+=[\"'].*?[\"']\\)"   ; Any optional key-value pairs like type="foo/bar"
                      "\\|\\(?:\\s-+scoped\\)"            ; The optional "scoped" attribute
                      "\\|\\(?:\\s-+module\\)"            ; The optional "module" attribute
                      "\\|\\(?:\\s-+setup\\)"             ; The optional "setup" attribute
                      "\\)*"
                      " *>\n"))                           ; The end of the tag
            (setq vue--front-tag-regex
              (concat "<%s"                        ; The tag name
                      "\\(?:"                      ; Zero of more of...
                      "\\(?:\\s-+" vue--not-lang-key "[\"'][^\"']*?[\"']\\)" ; Any optional key-value pairs like type="foo/bar".
                      ;; ^ Disallow "lang" in k/v pairs to avoid matching regions with non-default languages
                      "\\|\\(?:\\s-+scoped\\)"      ; The optional "scoped" attribute
                      "\\|\\(?:\\s-+module\\)"      ; The optional "module" attribute
                      "\\|\\(?:\\s-+setup\\)"       ; The optional "setup" attribute
                      "\\)*"
                      "\\s-*>\n"))                  ; The end of the tag
            )
          
          ;; HTML (and HTML template) support
          (use-package web-mode
            :mode "\\.html\\'"
            :mode "\\.html\\.erb\\'"
            )

          ;; YAML syntax support
          (use-package yaml-mode
            :mode "\\.yml\\'"
            :mode "\\.yaml\\'"
            )
        ''
      ];
      development.git.enable = lib.mkDefault true;
    };
  };
}
