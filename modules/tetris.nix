{ config, lib, pkgs, ... }:

let
  tetris = pkgs.mkYarnPackage rec {
    pname = "tetris";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "chvp";
      repo = "tetris";
      rev = "a3ff63bb8aecd42241a40ac8f7bdb6acacef7038";
      sha256 = "0nlj268kwbv45gsmsp2rc2bfdmbiks21pr8zsa22nfkpbm6m4c03";
    };
    yarnNix = ./tetris/yarn.nix;
    buildPhase = "yarn run build";
    installPhase = ''
      cp -r deps/tetris/dist $out
      rm $out/*.map
    '';
    distPhase = "true";
  };
in
{
  options.chvp.tetris.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.tetris.enable {
    chvp.nginx.hosts = [{
      fqdn = "tetris.vanpetegem.me";
      options = {
        root = "${tetris}";
        locations."/".index = "index.html";
      };
    }];
  };
}
