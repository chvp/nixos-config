let
  pkgs = import <nixpkgs> {};
  baseVimConfig = import ../programs/neovim/base.nix { inherit pkgs; };
  jdtls = import ../packages/jdtls/default.nix { inherit pkgs; stdenv = pkgs.stdenv; };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    jdk11
    jdtls
    openjfx11
    (
      pkgs.writeScriptBin "pmd" ''
        #!${pkgs.zsh}/bin/zsh

        ${pkgs.pmd}/bin/run.sh pmd $@
      ''
    )
    (
      neovim.override {
        configure = {
          customRC = baseVimConfig.customRC + ''
            " Required for operations modifying multiple buffers like rename
            set hidden

            let g:LanguageClient_serverCommands = {
            \ 'java': ['${jdtls}/bin/jdtls'],
            \ }

            let g:ale_linters = {
            \ 'java': ['pmd'],
            \}
          '';
          vam.knownPlugins = baseVimConfig.vam.knownPlugins;
          vam.pluginDictionaries = (baseVimConfig.vam.pluginDictionaries or []) ++ [
            {
              names = [
                "LanguageClient-neovim"
              ];
            }
          ];
        };
      }
    )
  ];
}
