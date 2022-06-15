{ pkgs, ... }: pkgs.devshell.mkShell {
  name = "Accentor API client in JavaScript";
  packages = with pkgs; [ nodejs yarn ];
}
