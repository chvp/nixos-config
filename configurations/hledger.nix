{ pkgs, ... }:
let
  hledger-repo = pkgs.fetchFromGitHub {
    owner = "simonmichael";
    repo = "hledger";
    rev = "76dd4d83bc114974fe498578971d9100be521d7a";
    sha256 = "109b52adhfsxh2zskjxy63xsw1mhn844ddq09b6llqz21h71wsmx";
  };
in
{
  nixpkgs.overlays = [
    (self: super: {
      haskellPackages = super.haskellPackages.override {
        overrides = hself: hsuper: rec {
          hledger = hsuper.callCabal2nixWithOptions "hledger" hledger-repo "--subpath hledger" { };
          hledger-lib = hsuper.callCabal2nixWithOptions "hledger-lib" hledger-repo "--subpath hledger-lib" { };
        };
      };
    })
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ pkgs.hledger ];
  };
}
