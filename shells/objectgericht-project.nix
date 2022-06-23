{ pkgs, ... }: pkgs.devshell.mkShell {
  name = "Objectgericht programmeren project";
  packages = with pkgs; [ openssl maven openjdk (python3.withPackages (ps: with ps; [ beautifulsoup4 notmuch pyyaml ])) ];
}
