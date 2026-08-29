{
  devshell,
  python3,
  hledger,
  ...
}:

devshell.mkShell {
  name = "Financials";
  packages = [
    (python3.withPackages (ps: [
      ps.requests
      ps.python-dateutil
    ]))
    hledger
  ];
}
