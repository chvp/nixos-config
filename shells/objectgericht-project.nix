{ pkgs, ... }: pkgs.devshell.mkShell {
  name = "Objectgericht programmeren project";
  packages = with pkgs; [
    openssl
    maven
    (openjdk17.override { enableJavaFX = true; })
  ];
  commands = [
    {
      name = "jdtls";
      category = "development";
      help = "Start jdt-language-server with configured workspace location";
      command = ''
        ${pkgs.jdt-language-server}/bin/jdt-language-server -data "$HOME/.cache/jdtls/$PRJ_ROOT"
      '';
    }
    {
      name = "start";
      category = "development";
      help = "Run game";
      command = "mvn clean javafx:run";
    }
    {
      name = "clean";
      category = "development";
      help = "Clean target";
      command = "mvn clean";
    }
  ];
  env = [
    {
    name = "LD_PRELOAD";
    value = "${pkgs.xorg.libXtst}/lib/libXtst.so.6";
    }
  ];
}
