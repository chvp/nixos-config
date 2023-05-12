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
      };
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs = {
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
    };
    devshell = {
      url = "github:chvp/devshell";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
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
    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/NUR";
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
      };
    };
  };

  outputs = inputs@{ self, nixpkgs, accentor, accentor-api, accentor-web, agenix, devshell, emacs-overlay, flake-utils, home-manager, nix-index-database, nixos-mailserver, nur, tetris, www-chvp-be }:
    let
      patches = builtins.map (patch: ./patches + "/${patch}") (builtins.filter (x: x != ".keep") (builtins.attrNames (builtins.readDir ./patches)));
      # Avoid IFD if there are no patches
      nixpkgsForSystem = system: if patches == [ ] then inputs.nixpkgs else
      (
        (import inputs.nixpkgs { inherit system; }).pkgs.applyPatches {
          inherit patches;
          name = "nixpkgs-patched-${inputs.nixpkgs.shortRev}";
          src = inputs.nixpkgs;
        });
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
      baseModules = [
        accentor.nixosModules.default
        agenix.nixosModules.age
        home-manager.nixosModule
        nixos-mailserver.nixosModule
        nix-index-database.nixosModules.nix-index
        ./modules
      ];
      nixosSystem = system: name: nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = baseModules ++ [
          ({ config, ... }:
            let nixpkgs = nixpkgsForSystem system; in
            {
              nixpkgs.pkgs = import nixpkgs { inherit overlays system; config = config.nixpkgs.config; };
              networking.hostName = name;
              nix = {
                extraOptions = "extra-experimental-features = nix-command flakes";
                registry = (builtins.mapAttrs (name: v: { flake = v; }) inputs) // { nixpkgs.flake = nixpkgs; };
              };
            })
          ./machines/${name}
        ];
      };
      nixosConfigurations = {
        kharbranth = nixosSystem "x86_64-linux" "kharbranth";
        kholinar = nixosSystem "x86_64-linux" "kholinar";
        lasting-integrity = nixosSystem "x86_64-linux" "lasting-integrity";
        urithiru = nixosSystem "x86_64-linux" "urithiru";
      };
      lsShells = builtins.readDir ./shells;
      shellFiles = builtins.filter (name: lsShells.${name} == "regular") (builtins.attrNames lsShells);
      shellNames = builtins.map (filename: builtins.head (builtins.split "\\." filename)) shellFiles;
      systemAttrs = flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = import (nixpkgsForSystem system) { inherit overlays system; };
          nameToValue = name: import (./shells + "/${name}.nix") { inherit pkgs inputs; };
        in
        {
          devShells = builtins.listToAttrs (builtins.map (name: { inherit name; value = nameToValue name; }) shellNames);
        }
      );
    in
    systemAttrs // { inherit nixosConfigurations; };
}
