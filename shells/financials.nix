{ pkgs, ... }:

pkgs.devshell.mkShell {
  name = "Financials";
  packages = with pkgs; [
    (python3.withPackages (ps: [ ps.requests ps.python-dateutil ]))
    hledger
  ];
}
