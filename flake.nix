{
  description = "Nixos configuration flake";

  inputs = {
    accentor = {
      url = "github:accentor/flake";
      inputs = {
        devshell.follows = "devshell";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    accentor-api = {
      url = "github:accentor/api";
      inputs = {
        devshell.follows = "devshell";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    accentor-web = {
      url = "github:accentor/web";
      inputs = {
        devshell.follows = "devshell";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-mailserver = {
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    tetris = {
      url = "github:chvp/tetris";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    utils = {
      url = "github:gytis-ivaskevicius/flake-utils-plus";
      inputs = {
        flake-utils.follows = "flake-utils";
        devshell.follows = "devshell";
      };
    };
  };

  outputs = inputs@{ self, nixpkgs, accentor, accentor-api, accentor-web, agenix, devshell, emacs-overlay, flake-utils, home-manager, nixos-mailserver, nur, tetris, utils }:
    let
      customPackages = callPackage: {
        jdtls = callPackage ./packages/jdtls { };
        kotlin-language-server = callPackage ./packages/kotlin-language-server { };
      }; in
    utils.lib.mkFlake {
      inherit self inputs;
      channels.nixpkgs = {
        input = nixpkgs;
        patches = map (patch: ./patches + "/${patch}") (builtins.filter (x: x != ".keep") (builtins.attrNames (builtins.readDir ./patches)));
        overlaysBuilder = _: [
          devshell.overlay
          emacs-overlay.overlay
          (self: super: customPackages self.callPackage)
          (self: super: {
            tetris = tetris.defaultPackage.${self.system};
            accentor-api = accentor-api.defaultPackage.${self.system};
            accentor-web = accentor-web.defaultPackage.${self.system};
          })
          nur.overlay
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
          home-manager.nixosModule
          nixos-mailserver.nixosModule
          ./modules
        ];
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
          devShells = rec {
            default = nixos-config;
            nixos-config = pkgs.devshell.mkShell {
              name = "NixOS config";
              packages = [
                pkgs.nixpkgs-fmt
                (pkgs.writeShellScriptBin "fetchpatch" "curl -L https://github.com/NixOS/nixpkgs/pull/$1.patch -o patches/$1.patch")
                agenix.defaultPackage.x86_64-linux
              ];
            };
          };
        };
    };
}
