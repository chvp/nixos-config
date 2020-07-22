{ pkgs, ... }:

{
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    package = pkgs.nixUnstable.overrideAttrs (oldAttrs: {
      src = pkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nix";
        rev = "ff314f186e3f91d87af6ad96c0ae3b472494b940";
        hash = "sha256-QibpLo4/gf2xYGoeQcgjZzH/qy5TBRVH+QCHgqOwur0=";
      };
    });
    gc = {
      automatic = true;
      dates = "hourly";
      options = "--delete-older-than 7d";
    };
    optimise = {
      automatic = true;
      dates = [ "hourly" ];
    };
    trustedUsers = [ "@wheel" ];
  };

  nixpkgs.config = import ./nix-store/config.nix;

  home-manager.users.charlotte = { ... }: {
    xdg.configFile."nixpkgs/config.nix".source = ./nix-store/config.nix;
  };
}
