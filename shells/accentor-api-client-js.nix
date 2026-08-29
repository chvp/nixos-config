{
  devshell,
  nodejs,
  ...
}:

devshell.mkShell {
  name = "Accentor API client in JavaScript";
  packages = [ nodejs ];
}
