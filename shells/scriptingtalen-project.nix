{ pkgs, ... }: pkgs.devshell.mkShell {
  name = "Scriptingtalen project";
  packages = with pkgs; [ (python3.withPackages (ps: with ps; [ beautifulsoup4 requests ])) ];
}
