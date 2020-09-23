{
  description = "Nixos configuration flake";

  inputs = {
    nixpkgs = { url = "github:charvp/nixpkgs/master"; };
    home-manager = {
      url = "github:rycee/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils = { url = "github:numtide/flake-utils/master"; };
  };

  outputs = { self, nixpkgs, home-manager, flake-utils }:
    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
      };
      mkSystem = system: hostname: nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          home-manager.nixosModules.home-manager
          (./. + "/machines/${hostname}")
          ({ pkgs, ... }: {
            environment.etc."nixpkgs".source = (pkgs.runCommandNoCC "nixpkgs" { } ''
              cp -r ${nixpkgs} $out
              chmod 700 $out
              echo "${nixpkgs.rev}" > $out/.version-suffix
            '');
            nix.nixPath = [ "nixpkgs=/etc/nixpkgs" ];
          })
        ];
      };
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = pkgsFor system;
        in
        {
          devShell = pkgs.mkShell {
            buildInputs = with nixpkgs.legacyPackages.${system}; [ nixpkgs-fmt ];
          };
        }) // {
      nixosConfigurations = {
        kharbranth = mkSystem "x86_64-linux" "kharbranth";
        kholinar = mkSystem "x86_64-linux" "kholinar";
      };
    };
}
