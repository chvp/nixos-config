{ config, lib, pkgs, ... }:

let
  hledger-repo = pkgs.fetchFromGitHub {
    owner = "simonmichael";
    repo = "hledger";
    rev = "master";
    sha256 = "5iDNRTMFehvUEc2Zeg7UWe+l9rdTdRoEO2xg6Q8Lmio=";
  };
in
{
  options.chvp.programs.hledger.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.programs.hledger.enable {
    chvp.base.emacs.extraConfig = [
      ''
        ;; Ledger syntax support
        (use-package ledger-mode
         :mode "\\.journal\\'"
         :custom
         (ledger-binary-path "hledger" "Use hledger instead of ledger")
         (ledger-highlight-xact-under-point nil "Remove distracting highlight")
         (ledger-mode-should-check-version nil "Remove version check, since it doesn't work with hledger anyway")
         (ledger-post-account-alignment-column 4 "Indent postings with 4 spaces")
         (ledger-post-amount-alignment-at :decimal "Align on the decimal")
         (ledger-post-amount-alignment-column 59 "Align on column 60")
         (ledger-post-auto-align t "Align when moving to the next line")
         )
      ''
    ];

    home-manager.users.charlotte = { ... }: {
      home.packages = [
        (pkgs.haskell.lib.justStaticExecutables (pkgs.haskellPackages.override {
          overrides = hself: hsuper: rec {
            hledger = hsuper.callCabal2nixWithOptions "hledger" hledger-repo "--subpath hledger" { };
            hledger-lib = hsuper.callCabal2nixWithOptions "hledger-lib" hledger-repo "--subpath hledger-lib" { };
            doctest = hsuper.doctest_0_18_1;
          };
        }).hledger)
      ];
    };
  };
}
