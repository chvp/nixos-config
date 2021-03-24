{
  description = "Nixos configuration flake";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus/staging";
  };

  outputs = inputs@{ self, nixpkgs, emacs-overlay, home-manager, utils }: utils.lib.systemFlake {
    inherit self inputs;
    channels.nixpkgs = {
      input = nixpkgs;
      patches = [ ];
    };
    sharedOverlays = [ emacs-overlay.overlay ];
    sharedModules = [
      ({ lib, ... }: {
        environment.etc = lib.mapAttrs' (key: val: { name = "channels/${key}"; value = { source = val.outPath; }; }) inputs;
        nix.nixPath = [ "/etc/channels" ];
      })
      utils.nixosModules.saneFlakeDefaults
      home-manager.nixosModules.home-manager
      ./modules
    ];
    nixosProfiles = {
      kharbranth = { modules = [ ./machines/kharbranth ]; };
      kholinar = { modules = [ ./machines/kholinar ]; };
      lasting-integrity = { modules = [ ./machines/lasting-integrity ]; };
      urithiru = { modules = [ ./machines/urithiru ]; };
    };
    devShellBuilder = channels:
      let pkgs = channels.nixpkgs; in pkgs.mkShell { buildInputs = [ pkgs.nixpkgs-fmt ]; };
  };
}
