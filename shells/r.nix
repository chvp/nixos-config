let
  pkgs = import <nixpkgs> { };
in
  pkgs.mkShell {
    buildInputs = [
      (pkgs.rWrapper.override {
        packages = with pkgs.rPackages; [
          base64enc
          jsonlite
          R6
          rlang
        ];
      })
    ];
  }
