let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs-12_x
    yarn
  ];
  shellHook = ''
    export PUPPETEER_EXECUTABLE_PATH="${pkgs.ungoogled-chromium}/bin/chromium"
  '';
}
