{ pkgs, ... }: pkgs.devshell.mkShell {
  name = "Objectgericht programmeren project";
  packages = with pkgs; [
    openssl maven openjdk17
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
      name = "client";
      category = "development";
      help = "Run client";
      command = "mvn package && java -cp target/dominion-1.0-SNAPSHOT.jar:repo/be/ugent/objprog/commhub/1.0/commhub-1.0.jar be.ugent.dominion.Main client";
    }
    {
      name = "server";
      category = "development";
      help = "Run server";
      command = "mvn package && java -cp target/dominion-1.0-SNAPSHOT.jar:repo/be/ugent/objprog/commhub/1.0/commhub-1.0.jar be.ugent.dominion.Main server";
    }
    {
      name = "clean";
      category = "development";
      help = "Clean target";
      command = "mvn clean";
    }
  ];
}
