{ pkgs, ... }:
let
  hledger-repo = pkgs.fetchFromGitHub {
    owner = "simonmichael";
    repo = "hledger";
    rev = "31868a6892928c02b385e327083d37f68adc047a";
    sha256 = "0n7b2vjc8gw10sypgvz4vkw0azgnkw208wpb41sgln95nxfd5dbc";
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
