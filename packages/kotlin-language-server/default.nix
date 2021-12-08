{ lib, stdenv, fetchzip, jdk11, makeWrapper, ... }:

stdenv.mkDerivation rec {
  pname = "kotlin-language-server";
  version = "1.2.0";
  src = fetchzip {
    url = "https://github.com/fwcd/${pname}/releases/download/${version}/server.zip";
    sha256 = "ooVEYL4ENf15NQWEU/5uQYD+s/RM2vTKDSpWUcccsPM=";
  };

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out
    cp -r $src/* $out/
    chmod -R u=rwX $out
    wrapProgram $out/bin/kotlin-language-server --set JAVA_HOME ${jdk11}
  '';
}
