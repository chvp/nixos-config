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
          sha256 = "/lVM1mKGX46qzelfjWUFd7RVlqpvCDzIhRBVmvM8EKA=";
        };
      });
      webPackage = (pkgs.accentor-web.override {
        packageJSON = ./package.json;
        yarnLock = ./yarn.lock;
        yarnNix = ./yarn.nix;
      }).overrideAttrs (old: {
        SKIP_CACHE = "true";
        src = pkgs.fetchFromGitHub {
          owner = "accentor";
          repo = "web";
          rev = "main";
          sha256 = "gYVJ9m9pjOu3dPwl2oBO1hlLEhPvquvuLP2GQytMsYM=";
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
