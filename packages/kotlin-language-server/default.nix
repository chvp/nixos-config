{ lib, stdenv, fetchzip, jdk17, makeWrapper, ... }:

stdenv.mkDerivation rec {
  pname = "kotlin-language-server";
  version = "1.3.1";
  src = fetchzip {
    url = "https://github.com/fwcd/${pname}/releases/download/${version}/server.zip";
    sha256 = "FxpNA4OGSgFdILl0yKBDTtVdQl6Bw9tm2eURbsJdZzI=";
  };

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out
    cp -r $src/* $out/
    chmod -R u=rwX $out
    wrapProgram $out/bin/kotlin-language-server --set JAVA_HOME ${jdk17}
  '';
}
