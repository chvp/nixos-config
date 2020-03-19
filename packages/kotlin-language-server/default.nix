{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  buildGradle = callPackage ./gradle-env.nix {};
in
buildGradle {
  envSpec = ./gradle-env.json;
  src = fetchTarball {
    url = "https://github.com/fwcd/kotlin-language-server/archive/master.tar.gz";
    sha256 = "0spcxrk6gyxrrwnffsmr2f5y8zs0nq1fq2pkscih56zxz0v3dm50";
  };
  gradleFlags = [ "server:installDist" ];
  installPhase = ''
    mkdir -p $out
    cp -r server/build/install/server/* $out/
  '';
}
