{ pkgs, ... }:

pkgs.devshell.mkShell {
  name = "Panic";
  packages = with pkgs; [
    python3
    pandoc
    texlive.combined.scheme-full
  ];
}
