{ config, lib, pkgs, ... }:

{
  options.chvp.services.accentor.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.accentor.enable {
    services.postgresql = {
      enable = true;
      dataDir = "${config.chvp.dataPrefix}/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
    };

    services.accentor = {
      enable = true;
      home = "${config.chvp.dataPrefix}/var/lib/accentor";
      hostname = "accentor.vanpetegem.me";
      workers = 4;
      environmentFile = config.age.secrets."passwords/services/accentor".path;
      rescanTimer = {
        enable = true;
        dates = "00:00";
      };
      nginx = {
        forceSSL = true;
        useACMEHost = "vanpetegem.me";
      };
      apiPackage = (pkgs.accentor-api.override {
        gemfile = ./Gemfile;
        lockfile = ./Gemfile.lock;
        gemset = ./gemset.nix;
      }).overrideAttrs (old: {
        src = pkgs.fetchFromGitHub {
          owner = "accentor";
          repo = "api";
          rev = "main";
          sha256 = "JfvnTbDE8pfaSf8QFuvqmKnq4EW0Vq8hxKnLIf34ovA=";
        };
      });
      webPackage = (pkgs.accentor-web.override {
        packageJSON = ./package.json;
        yarnLock = ./yarn.lock;
        yarnNix = ./yarn.nix;
      }).overrideAttrs (old: {
        IN_NIX = "true";
        patches = (old.patches or [ ]) ++ [ ./no_caching_in_nix.patch ];
        src = pkgs.fetchFromGitHub {
          owner = "accentor";
          repo = "web";
          rev = "main";
          sha256 = "alEGPio1uS5pFQ+riR2TWV2BIDYMRGgz1YHonhSECy4=";
        };
      });
    };

    security.doas.extraRules = [{
      users = [ "charlotte" ];
      noPass = true;
      cmd = "accentor-console";
      runAs = "accentor";
    }];

    age.secrets."passwords/services/accentor" = {
      file = ../../../secrets/passwords/services/accentor.age;
      owner = "accentor";
    };
  };
}
