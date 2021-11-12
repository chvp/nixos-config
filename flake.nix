{
  description = "Nixos configuration flake";

  inputs = {
    accentor = {
      url = "github:accentor/flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus/1.3.0";
  };

  outputs = inputs@{ self, nixpkgs, accentor, agenix, emacs-overlay, home-manager, utils }:
    let
      customPackages = callPackage: {
        jdtls = callPackage ./packages/jdtls { };
        kotlin-language-server = callPackage ./packages/kotlin-language-server { };
      }; in
    utils.lib.systemFlake {
      inherit self inputs;
      channels.nixpkgs = {
        input = nixpkgs;
        patches = map (patch: ./patches + "/${patch}") (builtins.filter (x: x != ".keep") (builtins.attrNames (builtins.readDir ./patches)));
        overlaysBuilder = _: [
          emacs-overlay.overlay
          (self: super: customPackages self.callPackage)
        ];
      };
      hostDefaults = {
        modules = [
          ({ lib, pkgs, ... }: {
            environment.etc = lib.mapAttrs'
              (key: val: {
                name = "channels/${key}";
                value = {
                  source = pkgs.runCommandNoCC "${key}-channel" { } ''
                    mkdir $out
                    echo "${val.rev or (toString val.lastModified)}" > $out/.version-suffix
                    echo "import ${val.outPath}/default.nix" > $out/default.nix
                  '';
                };
              })
              inputs;
            nix.nixPath = [ "/etc/channels" ];
          })
          accentor.nixosModule
          agenix.nixosModules.age
          home-manager.nixosModules.home-manager
          ./modules
        ];
        extraArgs = { nixosConfigurations = self.nixosConfigurations; };
      };
      hosts = {
        kharbranth.modules = [ ./machines/kharbranth ];
        kholinar.modules = [ ./machines/kholinar ];
        lasting-integrity.modules = [ ./machines/lasting-integrity ];
        urithiru.modules = [ ./machines/urithiru ];
      };
      outputsBuilder = channels:
        let pkgs = channels.nixpkgs; in
        {
          packages = customPackages pkgs.callPackage;
          devShell = pkgs.mkShell {
            buildInputs = [
              pkgs.nixpkgs-fmt
              (pkgs.writeShellScriptBin "fetchpatch" "curl -L https://github.com/NixOS/nixpkgs/pull/$1.patch -o patches/$1.patch")
              agenix.defaultPackage.x86_64-linux
            ];
          };
        };
    };
}
