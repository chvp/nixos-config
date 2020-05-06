let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs-12_x
    python38
    yarn
  ];
}
