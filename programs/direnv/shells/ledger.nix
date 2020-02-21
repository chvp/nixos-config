let
  pkgs = import <nixpkgs> {};
  nixpkgs-master = import <nixpkgs-master> {};
  baseVimConfig = import ./vim-base.nix { inherit pkgs; };
in
  pkgs.mkShell {
    buildInputs = with nixpkgs-master; [
      (neovim.override {
        configure = {
          customRC = baseVimConfig.customRC;
          vam.pluginDictionaries = (baseVimConfig.vam.pluginDictionaries or []) ++ [ { name = "vim-ledger"; } ];
        };
      })
    ];
  }
