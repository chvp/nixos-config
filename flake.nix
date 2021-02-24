{
  description = "Nixos configuration flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils/master";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:charvp/nixpkgs/master";
    nixpkgsFor0AD.url = "github:charvp/nixpkgs/0ad0.24";
  };

  outputs = { self, nixpkgs, nixpkgsFor0AD, home-manager, flake-utils }:
    let
      version-suffix = nixpkgs.rev or (builtins.toString nixpkgs.lastModified);
      pkgsFor = system: import nixpkgs {
        inherit system;
      };
      mkSystem = system: hostname: nixpkgs.lib.nixosSystem {
        inherit system;
        extraArgs = { pkgsFor0AD = import nixpkgsFor0AD { inherit system; }; };
        modules = [
          home-manager.nixosModules.home-manager
          (./modules)
          ({ pkgs, ... }: {
            environment.etc."nixpkgs".source = (pkgs.runCommandNoCC "nixpkgs" { } ''
              cp -r ${nixpkgs} $out
              chmod 700 $out
              echo "${version-suffix}" > $out/.version-suffix
            '');
            nix.nixPath = [ "nixpkgs=/etc/nixpkgs" ];
          })
          (./. + "/machines/${hostname}")
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
            buildInputs = with pkgs; [ nixpkgs-fmt ];
          };
        }) // {
      nixosConfigurations = {
        kharbranth = mkSystem "x86_64-linux" "kharbranth";
        kholinar = mkSystem "x86_64-linux" "kholinar";
        lasting-integrity = mkSystem "x86_64-linux" "lasting-integrity";
        urithiru = mkSystem "x86_64-linux" "urithiru";
      };
    };
}
