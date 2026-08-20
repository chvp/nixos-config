{ devshell
, python314
, nodejs_24
, yarn
, ...
}:

devshell.mkShell {
  name = "Papyros code editor";
  packages = [
    (python314.withPackages (ps: [ ps.pip ]))
    nodejs_24
    yarn
  ];
}
