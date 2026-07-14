{ pkgs, ... }:

pkgs.devshell.mkShell {
  name = "Curriculum vitae";
  packages = with pkgs; [
    texliveFull
    gnumake
    pandoc
  ];
}
