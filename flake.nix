{
  description = "Nixos configuration flake";

  inputs = {
    accentor = {
      url = "github:accentor/flake";
      inputs = {
        api.follows = "accentor-api";
        devshell.follows = "devshell";
        nixpkgs.follows = "nixpkgs";
        web.follows = "accentor-web";
      };
    };
    accentor-api = {
      url = "github:accentor/api";
      inputs = {
        devshell.follows = "devshell";
        nixpkgs.follows = "nixpkgs";
      };
    };
    accentor-desktop = {
      url = "github:accentor/desktop";
      inputs = {
        devshell.follows = "devshell";
        nixpkgs.follows = "nixpkgs";
      };
    };
    accentor-web = {
      url = "github:accentor/web";
      inputs = {
        devshell.follows = "devshell";
        nixpkgs.follows = "nixpkgs";
      };
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    de-lijst = {
      url = "git+https://git.chvp.be/chvp/de-lijst";
      inputs = {
        devshell.follows = "devshell";
        nixpkgs.follows = "nixpkgs";
      };
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    microvm = {
      url = "github:microvm-nix/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-mailserver = {
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable/nixexprs.tar.zst";
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware = {
      url = "github:nixos/nixos-hardware";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rycee-nur-expressions = {
      url = "gitlab:rycee/nur-expressions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    tetris = {
      url = "github:chvp/tetris";
      inputs = {
        devshell.follows = "devshell";
        nixpkgs.follows = "nixpkgs";
      };
    };
    www-chvp-be = {
      url = "git+https://git.chvp.be/chvp/www.chvp.be";
      inputs = {
        devshell.follows = "devshell";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs =
    inputs:
    let
      patches = builtins.map (patch: ./patches + "/${patch}") (
        builtins.filter (x: x != ".keep") (builtins.attrNames (builtins.readDir ./patches))
      );
      # Avoid IFD if there are no patches
      nixpkgsForSystem =
        system:
        if patches == [ ] then
          inputs.nixpkgs
        else
          (
            ((import inputs.nixpkgs { inherit system; }).pkgs.applyPatches {
              inherit patches;
              name = "nixpkgs-patched-${inputs.nixpkgs.shortRev}";
              src = inputs.nixpkgs;
            }).overrideAttrs
              (old: {
                preferLocalBuild = false;
                allowSubstitutes = true;
              })
          );
      overlay = inputs.nixpkgs.lib.composeManyExtensions [
        inputs.agenix.overlays.default
        inputs.accentor.overlays.default
        inputs.devshell.overlays.default
        inputs.emacs-overlay.overlays.default
        inputs.www-chvp-be.overlays.default
        (final: prev: {
          firefox-addons = inputs.rycee-nur-expressions.legacyPackages.${final.stdenv.hostPlatform.system}.firefox-addons;
          accentor-desktop = inputs.accentor-desktop.packages.${final.stdenv.hostPlatform.system}.default;
          de-lijst = inputs.de-lijst.packages.${final.stdenv.hostPlatform.system}.default;
          tetris = inputs.tetris.packages.${final.stdenv.hostPlatform.system}.default;
        })
      ];
      module = {
        imports = [ ./modules ];
      };
      nixosSystem =
        system: name: extraModules:
        let
          nixpkgs = nixpkgsForSystem system;
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ overlay ];
            config = {
              allowUnfreePredicate =
                pkg:
                builtins.elem (lib.getName pkg) [
                  "google-chrome"
                  "minecraft-launcher"
                  "minecraft-server"
                  "nvidia-kernel-modules"
                  "nvidia-settings"
                  "nvidia-x11"
                  "steam"
                  "steam-original"
                  "steam-run"
                  "steam-runtime"
                  "steam-unwrapped"
                ];
              permittedInsecurePackages = [ "olm-3.2.16" ];
            };
          };
          lib = (
            pkgs.lib.extend (
              final: prev: {
                hm = inputs.home-manager.lib.hm;
              }
            )
          );
        in
        inputs.nixpkgs.lib.nixosSystem {
          inherit lib system;
          specialArgs = {
            modulesPath = toString (nixpkgs + "/nixos/modules");
            unstablePkgs = pkgs;
          };
          baseModules = import (nixpkgs + "/nixos/modules/module-list.nix");
          modules = [
            inputs.accentor.nixosModules.default
            inputs.agenix.nixosModules.default
            inputs.home-manager.nixosModules.default
            inputs.microvm.nixosModules.host
            inputs.nixos-mailserver.nixosModules.default
            inputs.nix-index-database.nixosModules.nix-index
            module
          ]
          ++ extraModules
          ++ [
            (
              { config, ... }:
              {
                _module.args = { inherit inputs; };
                nixpkgs = {
                  inherit pkgs;
                  flake.source = lib.mkForce "${nixpkgs}";
                };
                networking.hostName = name;
                nix = {
                  extraOptions = "extra-experimental-features = nix-command flakes";
                  registry.nixpkgs.flake = nixpkgs;
                };
              }
            )
            ./machines/${name}
          ];
        };
      nixosConfigurations = {
        elendel = nixosSystem "x86_64-linux" "elendel" [ ];
        kholinar = nixosSystem "x86_64-linux" "kholinar" [
          inputs.nixos-hardware.nixosModules.framework-amd-ai-300-series
        ];
        marabethia = nixosSystem "x86_64-linux" "marabethia" [ ];
        purelake = nixosSystem "x86_64-linux" "purelake" [ ];
      };
      lsShells = builtins.readDir ./shells;
      shellFiles = builtins.filter (name: lsShells.${name} == "regular") (builtins.attrNames lsShells);
      shellNames = builtins.map (filename: builtins.head (builtins.split "\\." filename)) shellFiles;
    in
    {
      inherit nixosConfigurations;
      nixosModules.default = module;
      overlays.default = overlay;
      devShells = builtins.mapAttrs (
        system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
        in
        builtins.listToAttrs (
          builtins.map (name: {
            inherit name;
            value = pkgs.callPackage (./shells + "/${name}.nix") { inputs = inputs; };
          }) shellNames
        )
      ) inputs.nixpkgs.legacyPackages;
    };
}
