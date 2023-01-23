{ config, lib, pkgs, ... }:

{
  options.chvp.services.git.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.git.enable {
    chvp.services.nginx.hosts = [{
      fqdn = "git.chvp.be";
      options = {
        locations."/".proxyPass = "http://unix:/run/gitlab/gitlab-workhorse.socket";
      };
    }];
    users = {
      users = {
        git = {
          uid = lib.mkForce 963;
          group = "git";
          isSystemUser = true;
          useDefaultShell = true;
        };
        nginx.extraGroups = [ "git" ];
      };
      groups.git.gid = lib.mkForce 963;
    };
    services.openssh.settings.AcceptEnv = "GIT_PROTOCOL";
    services.gitlab = {
      enable = true;
      statePath = "/var/lib/git/state";
      backup.path = "/var/lib/git/backup";
      databaseCreateLocally = true;
      databaseUsername = "git";
      databaseName = "git";
      user = "git";
      group = "git";
      host = "git.chvp.be";
      port = 443;
      https = true;
      initialRootEmail = "charlotte@vanpetegem.me";
      initialRootPasswordFile = config.age.secrets."passwords/services/git/initial-root-password".path;
      secrets = {
        dbFile = config.age.secrets."passwords/services/git/db".path;
        jwsFile = config.age.secrets."passwords/services/git/jws".path;
        otpFile = config.age.secrets."passwords/services/git/otp".path;
        secretFile = config.age.secrets."passwords/services/git/secret".path;
      };
      smtp = {
        enable = true;
        enableStartTLSAuto = false;
      };
    };

    age.secrets."passwords/services/git/initial-root-password" = {
      file = ../../../secrets/passwords/services/git/initial-root-password.age;
      owner = "git";
    };
    age.secrets."passwords/services/git/db" = {
      file = ../../../secrets/passwords/services/git/db.age;
      owner = "git";
    };
    age.secrets."passwords/services/git/jws" = {
      file = ../../../secrets/passwords/services/git/jws.age;
      owner = "git";
    };
    age.secrets."passwords/services/git/otp" = {
      file = ../../../secrets/passwords/services/git/otp.age;
      owner = "git";
    };
    age.secrets."passwords/services/git/secret" = {
      file = ../../../secrets/passwords/services/git/secret.age;
      owner = "git";
    };
  };
}
