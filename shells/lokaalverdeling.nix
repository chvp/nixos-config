{ pkgs, ... }:

let
  ezodf = pkgs.python3.pkgs.buildPythonPackage rec {
    pname = "ezodf";
    version = "0.3.2";

    src = pkgs.python3.pkgs.fetchPypi {
      inherit pname version;
      hash = "sha256-AA2lNPaJxtVSl6CPni7X6tqYENGU0x0WQ4gWL7OREi0=";
    };

    buildInputs = [ pkgs.python3.pkgs.lxml ];

    doCheck = false;
  };
  pandas-ods-reader = pkgs.python3.pkgs.buildPythonPackage rec {
    pname = "pandas-ods-reader";
    version = "0.1.4";

    src = pkgs.python3.pkgs.fetchPypi {
      inherit pname version;
      hash = "sha256-0J5Xr07EQObV3xJ5USdpWy2RFlfSAoQ/ELFZS2gtPAk=";
    };

    propagatedBuildInputs = [ ezodf pkgs.python3.pkgs.pandas pkgs.python3.pkgs.lxml ];

    doCheck = false;
  };
in
pkgs.devshell.mkShell {
  name = "Lokaalverdeling";
  packages = with pkgs; [
    (python3.withPackages (ps: with ps; [ click pandas-ods-reader jinja2 pyyaml ]))
  ];
}
