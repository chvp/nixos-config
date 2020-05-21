let
  pkgs = import <nixpkgs> { };
  extraRpath = pkgs.stdenv.lib.strings.makeLibraryPath (with pkgs; [ ffmpeg ]);
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    (
      pkgs.writeScriptBin "java" ''
        #!${pkgs.zsh}/bin/zsh

        old_path="$(patchelf --print-rpath ${jdk11}/bin/java)"
        LD_LIBRARY_PATH="$old_path:${extraRpath}" ${jdk11}/bin/java $@
      ''
    )
    jdk11
    openjfx11
  ];
}
