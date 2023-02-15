{ pkgs, ... }: pkgs.devshell.mkShell {
  name = "Objectgericht programmeren project";
  packages = with pkgs; [
    openssl maven openjdk17
  ];
  commands = [
    {
      name = "jdtls";
      category = "language server";
      help = "Start jdt-language-server with configured workspace location";
      command = ''
        ${pkgs.jdt-language-server}/bin/jdt-language-server -data "$PRJ_DATA_DIR/workspace"
      '';
    }
  ];
}
