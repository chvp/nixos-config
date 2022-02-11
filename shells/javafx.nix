let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    (
      pkgs.writeShellScriptBin "compile" ''
        find src -name '*.java' -print0 | xargs --no-run-if-empty -0 ${jdk17}/bin/javac -d out -sourcepath src
        find src -type d -printf '%P\0' | xargs -0 -I \{\} mkdir -p out/\{\}
        find resources -type d -printf '%P\0' | xargs -0 -I \{\} mkdir -p out/\{\}
        find src -type f -not -name '*.java' -printf '%P\0' | xargs -0 -I \{\} cp src/\{\} out/\{\}
        find resources -type f -not -name '*.java' -printf '%P\0' | xargs -0 -I \{\} cp resources/\{\} out/\{\}
        (cd out && ${jdk17}/bin/java polis.Main)
      ''
    )
    jdk17
    openjfx17
  ];
}
