{ pkgs, ... }: pkgs.devshell.mkShell {
  name = "Papyros code editor";
  packages = with pkgs; [ nodejs yarn (python3.withPackages (ps: [ ps.pip ])) ];
}
