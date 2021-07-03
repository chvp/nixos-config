{ lib, stdenv, fetchzip, jdk11, makeWrapper, ... }:

stdenv.mkDerivation rec {
  pname = "kotlin-language-server";
  version = "1.1.2";
  src = fetchzip {
    url = "https://github.com/fwcd/${pname}/releases/download/${version}/server.zip";
    sha256 = "021h9239lr19r9r726hfjlfgwa8fl4m8vfryzsg8fbm0hsziklkz";
  };

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out
    cp -r $src/* $out/
    chmod -R u=rwX $out
    wrapProgram $out/bin/kotlin-language-server --set JAVA_HOME ${jdk11}
  '';
}
