let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = [
    (pkgs.rWrapper.override {
      packages = with pkgs.rPackages; [
        base64enc
        dplyr
        dslabs
        jsonlite
        knitr
        lintr
        R6
        rlang
        styler
      ];
    })
  ];
}
