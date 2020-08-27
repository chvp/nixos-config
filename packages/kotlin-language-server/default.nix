{ pkgs ? import <nixpkgs> { } }:

with pkgs;
let
  buildGradle = callPackage ./gradle-env.nix { };
in
buildGradle {
  envSpec = ./gradle-env.json;
  src = fetchTarball {
    url = "https://github.com/fwcd/kotlin-language-server/archive/0.7.0.tar.gz";
    sha256 = "0by07h2ly84dzmwzjf3fsgghm3fwhyhhbnnv3kl7dy1iajhl4shj";
  };
  gradleFlags = [ "server:installDist" ];
  installPhase = ''
    mkdir -p $out
    cp -r server/build/install/server/* $out/
  '';
}
