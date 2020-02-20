let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      (neovim.override {
        configure = {
          customRC = baseVimConfig.customRC;
          vam.pluginDictionaries = (baseVimConfig.vam.pluginDictionaries or []);
        };
      })
    ];
  }
