{ pkgs, ... }:
let
  hledger-repo = pkgs.fetchFromGitHub {
    owner = "simonmichael";
    repo = "hledger";
    rev = "540c65994c65c909bf1e365aa0c05671636ac1c7";
    sha256 = "1sfs6psla1x5243c3kpkhiaskq5jf890lwcqjqbqrrk69il7d8s2";
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
