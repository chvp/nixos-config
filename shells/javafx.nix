let
  pkgs = import <nixpkgs> { };
  extraRpath = pkgs.stdenv.lib.strings.makeLibraryPath (with pkgs; [ ffmpeg ]);
  java = pkgs.writeShellScriptBin "java" ''
    old_path="$(patchelf --print-rpath ${pkgs.jdk11}/bin/java)"
    LD_LIBRARY_PATH="$old_path:${extraRpath}" ${pkgs.jdk11}/bin/java $@
  '';
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    java
    (
      pkgs.writeShellScriptBin "compile" ''
        find src -name '*.java' -print0 | xargs --no-run-if-empty -0 ${jdk11}/bin/javac -d out -sourcepath src
        find src -type d -printf '%P\0' | xargs -0 -I \{\} mkdir -p out/\{\}
        find resources -type d -printf '%P\0' | xargs -0 -I \{\} mkdir -p out/\{\}
        find src -type f -not -name '*.java' -printf '%P\0' | xargs -0 -I \{\} cp src/\{\} out/\{\}
        find resources -type f -not -name '*.java' -printf '%P\0' | xargs -0 -I \{\} cp resources/\{\} out/\{\}
        (cd out && ${java}/bin/java polis.Main)
      ''
    )
    jdk11
    openjfx11
  ];
}
