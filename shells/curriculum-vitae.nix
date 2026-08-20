{ devshell
, texliveFull
, gnumake
, pandoc
, ...
}:

devshell.mkShell {
  name = "Curriculum vitae";
  packages = [
    texliveFull
    gnumake
    pandoc
  ];
}
