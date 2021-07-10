{ config, lib, pkgs, ... }:

let
  tetris = pkgs.mkYarnPackage rec {
    pname = "tetris";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "chvp";
      repo = "tetris";
      rev = "master";
      sha256 = "0jq5pih8bb1w2pglpklqix01pdd3a3wzzf0jnai0wy5gn93abpik";
    };
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
    yarnNix = ./yarn.nix;
    buildPhase = "yarn run build";
    installPhase = ''
      cp -r deps/tetris/dist $out
      rm $out/*.map
    '';
    distPhase = "true";
  };
in
{
  options.chvp.games.tetris.server = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.games.tetris.server {
    chvp.services.nginx.hosts = [{
      fqdn = "tetris.vanpetegem.me";
      options = {
        root = "${tetris}";
        locations."/".index = "index.html";
      };
    }];
  };
}
