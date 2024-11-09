{
  description = "Nixos configuration flake";

  inputs = {
    accentor = {
      url = "github:accentor/flake";
      inputs = {
        api.follows = "accentor-api";
        devshell.follows = "devshell";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
        web.follows = "accentor-web";
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
        systems.follows = "systems";
      };
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs = {
        darwin.follows = "darwin";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };
    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devshell = {
      url = "github:chvp/devshell/ruby-darwin-fix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    lanzaboote = {
      url = "github:nix-community/lanzaboote/v0.3.0";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    nixos-mailserver = {
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
    systems.url = "github:nix-systems/default";
    tetris = {
      url = "github:chvp/tetris";
      inputs = {
        devshell.follows = "devshell";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    www-chvp-be = {
      url = "gitlab:chvp/www.chvp.be?host=git.chvp.be";
      inputs = {
        devshell.follows = "devshell";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };
  };

  outputs = inputs@{ self, nixpkgs, accentor, accentor-api, accentor-web, agenix, darwin, devshell, emacs-overlay, flake-utils, home-manager, lanzaboote, nix-index-database, nixos-mailserver, nur, tetris, www-chvp-be, ... }:
    let
      patches = builtins.map (patch: ./patches + "/${patch}") (builtins.filter (x: x != ".keep") (builtins.attrNames (builtins.readDir ./patches)));
      # Avoid IFD if there are no patches
      nixpkgsForSystem = system: if patches == [ ] then inputs.nixpkgs else
      (
        ((import inputs.nixpkgs { inherit system; }).pkgs.applyPatches {
          inherit patches;
          name = "nixpkgs-patched-${inputs.nixpkgs.shortRev}";
          src = inputs.nixpkgs;
        }).overrideAttrs (old: {
          preferLocalBuild = false;
          allowSubstitutes = true;
        })
      );
      overlays = [
        agenix.overlays.default
        accentor.overlays.default
        devshell.overlays.default
        emacs-overlay.overlay
        (self: super: {
          tetris = tetris.packages.${self.system}.default;
        })
        nur.overlay
        www-chvp-be.overlays.default
      ];
      commonModules = [
        ./modules/shared
      ];
      nixosModules = [
        accentor.nixosModules.default
        agenix.nixosModules.default
        home-manager.nixosModules.default
        lanzaboote.nixosModules.lanzaboote
        nixos-mailserver.nixosModules.default
        nix-index-database.nixosModules.nix-index
        ./modules/nixos
      ];
      darwinModules = [
        agenix.darwinModules.default
        home-manager.darwinModules.default
        ./modules/darwin
      ];
      nixosSystem = system: name:
        let
          nixpkgs = nixpkgsForSystem system;
          lib = (import nixpkgs { inherit overlays system; }).lib;
        in
        inputs.nixpkgs.lib.nixosSystem {
          inherit lib system;
          specialArgs = { modulesPath = toString (nixpkgs + "/nixos/modules"); };
          baseModules = import (nixpkgs + "/nixos/modules/module-list.nix");
          modules = commonModules ++ nixosModules ++ [
            ({ config, ... }:
              {
                nixpkgs = {
                  pkgs = import nixpkgs {
                    inherit overlays system;
                    config = {
                      allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) config.chvp.base.nix.unfreePackages;
                      permittedInsecurePackages = [ "olm-3.2.16" ];
                    };
                  };
                  flake.source = lib.mkForce "${nixpkgs}";
                };
                networking.hostName = name;
                nix = {
                  extraOptions = "extra-experimental-features = nix-command flakes";
                  registry = (builtins.mapAttrs (name: v: { flake = v; }) inputs) // { nixpkgs = { flake = nixpkgs; }; };
                };
              })
            ./machines/${name}
          ];
        };
      darwinSystem = system: name:
        let
          nixpkgs = nixpkgsForSystem system;
          lib = (import nixpkgs { inherit overlays system; }).lib;
        in
        darwin.lib.darwinSystem {
          inherit lib system;
          modules = commonModules ++ darwinModules ++ [
            ({ config, ... }:
              {
                nixpkgs.pkgs = import nixpkgs {
                  inherit overlays system;
                  config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) config.chvp.base.nix.unfreePackages;
                };
                networking.hostName = name;
                nix = {
                  extraOptions = "extra-experimental-features = nix-command flakes";
                  registry = (builtins.mapAttrs (name: v: { flake = v; }) inputs) // { nixpkgs = { flake = nixpkgs; }; };
                };
              })
            ./machines/${name}
            home-manager.darwinModules.home-manager
          ];
        };
      nixosConfigurations = {
        kholinar = nixosSystem "x86_64-linux" "kholinar";
        lasting-integrity = nixosSystem "x86_64-linux" "lasting-integrity";
        marabethia = nixosSystem "x86_64-linux" "marabethia";
        urithiru = nixosSystem "x86_64-linux" "urithiru";
      };
      darwinConfigurations.thaylen-city = darwinSystem "aarch64-darwin" "thaylen-city";
      lsShells = builtins.readDir ./shells;
      shellFiles = builtins.filter (name: lsShells.${name} == "regular") (builtins.attrNames lsShells);
      shellNames = builtins.map (filename: builtins.head (builtins.split "\\." filename)) shellFiles;
      systemAttrs = flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = import (nixpkgsForSystem system) { inherit overlays system; config.permittedInsecurePackages = [ "imagemagick-6.9.13-10" ]; };
          lib = pkgs.lib;
          nameToValue = name: import (./shells + "/${name}.nix") { inherit lib pkgs inputs system; };
        in
        {
          devShells = builtins.listToAttrs (builtins.map (name: { inherit name; value = nameToValue name; }) shellNames);
        }
      );
    in
    systemAttrs // { inherit nixosConfigurations darwinConfigurations; };
}
