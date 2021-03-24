{
  description = "Nixos configuration flake";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus/master";
  };

  outputs = inputs@{ self, nixpkgs, emacs-overlay, home-manager, utils }: utils.lib.systemFlake {
    inherit self inputs;
    channels.nixpkgs = {
      input = nixpkgs;
      patches = [ ];
      # TODO: Try to find a way to get rid of this and return to the
      # list built up in the config.
      config = { allowUnfree = true; };
    };
    sharedOverlays = [ emacs-overlay.overlay ];
    sharedModules = [
      utils.nixosModules.saneFlakeDefaults
      home-manager.nixosModules.home-manager
      ./modules
    ];
    nixosProfiles = {
      kharbranth = { system = "x86_64-linux"; modules = [ ./machines/kharbranth ]; };
      kholinar = { system = "x86_64-linux"; modules = [ ./machines/kholinar ]; };
      lasting-integrity = { system = "x86_64-linux"; modules = [ ./machines/lasting-integrity ]; };
      urithiru = { system = "x86_64-linux"; modules = [ ./machines/urithiru ]; };
    };
    devShellBuilder = channels:
      let pkgs = channels.nixpkgs; in pkgs.mkShell { buildInputs = [ pkgs.nixpkgs-fmt ]; };
  };
}
