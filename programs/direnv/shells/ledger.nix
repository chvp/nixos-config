let
  pkgs = import <nixpkgs> {};
  baseVimConfig = import ./vim-base.nix { inherit pkgs; };
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      (neovim.override {
        configure = {
          customRC = baseVimConfig.customRC;
          vam.pluginDictionaries = (baseVimConfig.vam.pluginDictionaries or []) ++ [ { name = "vim-ledger"; } ];
        };
      })
    ];
  }
