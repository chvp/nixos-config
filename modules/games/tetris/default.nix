{ config, lib, pkgs, ... }:

let
  tetris = pkgs.mkYarnPackage rec {
    pname = "tetris";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "chvp";
      repo = "tetris";
      rev = "main";
      sha256 = "lH7LV03pRCJnY4ZjklWpmNuWjrQUiy1LwuByQlA1nTg=";
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
