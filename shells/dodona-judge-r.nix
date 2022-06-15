{ pkgs, ... }: pkgs.devshell.mkShell {
  name = "R judge";
  packages = [
    (pkgs.rWrapper.override {
      packages = with pkgs.rPackages; [ base64enc dplyr dslabs jsonlite knitr lintr R6 rlang styler ];
    })
  ];
}
