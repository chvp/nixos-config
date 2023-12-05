{ pkgs, ... }:

pkgs.devshell.mkShell {
  name = "Curriculum vitae";
  packages = with pkgs; [
    texlive.combined.scheme-full
  ];
}
