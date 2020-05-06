{ pkgs ? import <nixpkgs> { } }:

with pkgs;
let
  buildGradle = callPackage ./gradle-env.nix { };
in
buildGradle {
  envSpec = ./gradle-env.json;
  src = fetchTarball {
    url = "https://github.com/fwcd/kotlin-language-server/archive/master.tar.gz";
    sha256 = "16qddjwcj1n79c4p9dqlj1vg6yg5rkvzb8r15jbyxsy82mp6ib3k";
  };
  gradleFlags = [ "server:installDist" ];
  installPhase = ''
    mkdir -p $out
    cp -r server/build/install/server/* $out/
  '';
}
