let
  pkgs = import <nixpkgs> {};
  baseVimConfig = import ../programs/neovim/base.nix { inherit pkgs; };
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      (neovim.override {
        configure = {
          customRC = baseVimConfig.customRC;
          vam.knownPlugins = baseVimConfig.vam.knownPlugins;
          vam.pluginDictionaries = (baseVimConfig.vam.pluginDictionaries or []) ++ [ { name = "vim-ledger"; } ];
        };
      })
    ];
  }
