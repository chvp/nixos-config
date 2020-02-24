let
  pkgs = import <nixpkgs> {};
  baseVimConfig = import ../programs/neovim/base.nix { inherit pkgs; };
  jdtls = import ../packages/jdtls/default.nix { inherit pkgs; stdenv = pkgs.stdenv; };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    jdk11
    openjfx11
    jdtls
    (
      neovim.override {
        configure = {
          customRC = baseVimConfig.customRC + ''
            " Required for operations modifying multiple buffers like rename
            set hidden

            let g:LanguageClient_serverCommands = {
            \ 'java': ['${jdtls}/bin/jdtls'],
            \ }
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
