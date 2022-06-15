{ pkgs, ... }: pkgs.devshell.mkShell {
  name = "Objectgericht programmeren project";
  packages = with pkgs; [ openssl maven openjdk ];
}
