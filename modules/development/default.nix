{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

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
      base = {
        emacs = {
          config = {
            editorconfig = lib.hm.dag.entryAnywhere {
              packages = epkgs: [ epkgs.editorconfig ];
              elisp = ''
                (require 'editorconfig)
                (editorconfig-mode 1)
                (diminish 'editorconfig-mode)
              '';
            };
            eglot = lib.hm.dag.entryAfter [ "flycheck" "general" "vue-mode" ] {
              packages = epkgs: [ epkgs.eglot ];
              elisp = ''
                (require 'eglot)
                (global-flycheck-eglot-mode)
                (lmap
                  :keymaps '(prog-mode-map vue-mode-map)
                  "SPC s" '("Add buffer to eglot" . eglot)
                  "SPC f" '("Format region" . eglot-format)
                  "SPC F" '("Format buffer" . eglot-format)
                  "SPC r" '("Rename symbol" . eglot-rename)
                  "SPC a" '("Relevant local actions" . eglot-code-actions)
                  "SPC n" '("Next error" . flycheck-next-error)
                  "SPC p" '("Previous error" . flycheck-prev-error)
                )
                (defun chvp--eglot-capf ()
                  (setq-local completion-at-point-functions
                                (list (cape-capf-buster #'eglot-completion-at-point #'string-prefix-p)
                                      #'tempel-complete
                                      #'cape-file
                                      #'dabbrev-capf
                                      #'cape-line)))
                (add-hook 'eglot-managed-mode-hook #'chvp--eglot-capf)
              '';
            };
            haskell-mode = lib.hm.dag.entryAnywhere {
              packages = epkgs: [ epkgs.haskell-mode ];
              elisp = ''
                (require 'haskell-mode)
                (require 'haskell-doc)
                (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
              '';
            };
            markdown = lib.hm.dag.entryAnywhere {
              packages = epkgs: [ epkgs.markdown-mode ];
              elisp = ''
                (require 'markdown-mode)
                (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
                (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
                (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
              '';
            };
            origami = lib.hm.dag.entryAnywhere {
              packages = epkgs: [ epkgs.origami ];
              elisp = ''
                (require 'origami)
                (add-hook 'prog-mode-hook #'origami-mode)
              '';
            };
            python-mode = lib.hm.dag.entryAnywhere {
              packages = epkgs: [ epkgs.python-mode ];
              elisp = ''
                (require 'python-mode)
                (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
              '';
            };
            r = lib.hm.dag.entryAnywhere {
              packages = epkgs: [ epkgs.ess ];
              elisp = ''
                (require 'ess-r-mode)
                (add-to-list 'auto-mode-alist '("\\.r\\'" . ess-r-mode))
                (add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))
              '';
            };
            ruby-mode = lib.hm.dag.entryAfter [ "eglot" ] {
              elisp = ''
                (setopt ruby-insert-encoding-magic-comment nil)
                (require 'ruby-mode)
                (add-to-list 'auto-mode-alist '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
                (add-to-list 'auto-mode-alist '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))
                (add-to-list 'eglot-server-programs `(ruby-mode . ("ruby-lsp")))
              '';
            };
            rust-mode = lib.hm.dag.entryAnywhere {
              packages = epkgs: [ epkgs.rust-mode ];
              elisp = ''
                (require 'rust-mode)
                (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
              '';
            };
            treesitter = lib.hm.dag.entryAnywhere {
              packages = epkgs: [ epkgs.treesit-grammars.with-all-grammars ];
              elisp = ''
                (setq treesit-auto-install-grammar nil)
              '';
            };
            typescript-mode = lib.hm.dag.entryAnywhere {
              packages = epkgs: [ epkgs.typescript-mode ];
              elisp = ''
                (require 'typescript-mode)
                (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
              '';
            };
            vue-mode = lib.hm.dag.entryAnywhere {
              packages = epkgs: [ epkgs.vue-mode ];
              elisp = ''
                (setopt mmm-submode-decoration-level 0)
                (require 'vue-mode)
                (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
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
              '';
            };
            web-mode = lib.hm.dag.entryAnywhere {
              packages = epkgs: [ epkgs.web-mode ];
              elisp = ''
                (require 'web-mode)
                (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
                (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . web-mode))
              '';
            };
            yaml-mode = lib.hm.dag.entryAnywhere {
              packages = epkgs: [ epkgs.yaml-mode ];
              elisp = ''
                (require 'yaml-mode)
                (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
                (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
              '';
            };
          };
        };
        zfs.homeLinks = [
          {
            path = "repos";
            type = "cache";
          }
        ];
      };
      development = {
        docker.enable = lib.mkDefault true;
        git.enable = lib.mkDefault true;
      };
    };
  };
}
