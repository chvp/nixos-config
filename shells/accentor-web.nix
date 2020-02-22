let
  pkgs = import <nixpkgs> {};
  baseVimConfig = import ../programs/neovim/base.nix { inherit pkgs; };
  nodePackages = import ../packages/node/default.nix { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs-12_x
    yarn
    (neovim.override {
      configure = {
          customRC = baseVimConfig.customRC + ''
            " Required for operations modifying multiple buffers like rename
            set hidden

            let g:LanguageClient_serverCommands = {
            \ 'vue': ['${nodePackages.vue-language-server}/bin/vls'],
            \ }
          '';
          vam.knownPlugins = baseVimConfig.vam.knownPlugins;
          vam.pluginDictionaries = (baseVimConfig.vam.pluginDictionaries or []) ++ [
            {
              names = [
                "LanguageClient-neovim"
                "vim-vue"
              ];
            }
          ];
      };
    })
  ];
}
