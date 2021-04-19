{
  description = "Nixos configuration flake";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay/d9530a7048f4b1c0f65825202a0ce1d111a1d39a";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
    utils.url = "github:gytis-ivaskevicius/flake-utils-plus/staging";
  };

  outputs = inputs@{ self, nixpkgs, emacs-overlay, home-manager, utils }: utils.lib.systemFlake {
    inherit self inputs;
    # This config can only be evaluated on x86_64-linux because of IFD
    supportedSystems = [ "x86_64-linux" ];
    channels.nixpkgs = {
      input = nixpkgs;
      patches = map (patch: ./patches + "/${patch}") (builtins.attrNames (builtins.readDir ./patches));
      overlaysBuilder = _: [ emacs-overlay.overlay ];
    };
    hostDefaults = {
      modules = [
        ({ lib, ... }: {
          environment.etc = lib.mapAttrs' (key: val: { name = "channels/${key}"; value = { source = val.outPath; }; }) inputs;
          nix.nixPath = [ "/etc/channels" ];
        })
        utils.nixosModules.saneFlakeDefaults
        home-manager.nixosModules.home-manager
        ./modules
      ];
    };
    hosts = {
      kharbranth.modules = [ ./machines/kharbranth ];
      kholinar.modules = [ ./machines/kholinar ];
      lasting-integrity.modules = [ ./machines/lasting-integrity ];
      urithiru.modules = [ ./machines/urithiru ];
    };
    devShellBuilder = channels:
      let pkgs = channels.nixpkgs; in
      pkgs.mkShell {
        buildInputs = [
          pkgs.nixpkgs-fmt
          (pkgs.writeShellScriptBin "fetchpatch" "curl -L https://github.com/NixOS/nixpkgs/pull/$1.patch -o patches/$1.patch")
        ];
      };
  };
}
