{ pkgs, lib, inputs, system, ... }: pkgs.devshell.mkShell {
  name = "Dodona exercise repo";
  packages = [ (pkgs.python3.withPackages (ps: [ ps.beautifulsoup4 ps.requests ])) ];
}
