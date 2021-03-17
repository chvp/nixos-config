{
  description = "Nixos configuration flake";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
    flake-utils.url = "github:numtide/flake-utils/master";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:chvp/nixpkgs/master";
    nixpkgsFor0AD.url = "github:chvp/nixpkgs/0ad0.24";
    nur.url = "github:nix-community/NUR";
  };

  outputs = { self, emacs-overlay, nixpkgs, nixpkgsFor0AD, nur, home-manager, flake-utils }:
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
          ({ pkgs, ... }: {
            nixpkgs.overlays = [ nur.overlay emacs-overlay.overlay ];
            home-manager.sharedModules = [ pkgs.nur.repos.rycee.hmModules.emacs-init ];
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
