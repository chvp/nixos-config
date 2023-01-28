{ config, lib, pkgs, ... }:

{
  options.chvp.services.git.runner.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.git.runner.enable {
    services.gitlab-runner = {
      enable = true;
      services = {
        nix = {
          registrationConfigFile = config.age.secrets."passwords/services/gitlab-runner/registration".path;
          dockerImage = "alpine";
          dockerVolumes = [
            "/nix/store:/nix/store:ro"
            "/nix/var/nix/db:/nix/var/nix/db:ro"
            "/nix/var/nix/daemon-socket:/nix/var/nix/daemon-socket:ro"
            "/etc/nix/nix.conf:/etc/nix/nix.conf:ro"
          ];
          dockerDisableCache = true;
          preBuildScript = pkgs.writeScript "setup-container" ''
            mkdir -p -m 0755 /nix/var/log/nix/drvs
            mkdir -p -m 0755 /nix/var/nix/gcroots
            mkdir -p -m 0755 /nix/var/nix/profiles
            mkdir -p -m 0755 /nix/var/nix/temproots
            mkdir -p -m 0755 /nix/var/nix/userpool
            mkdir -p -m 1777 /nix/var/nix/gcroots/per-user
            mkdir -p -m 1777 /nix/var/nix/profiles/per-user
            mkdir -p -m 0755 /nix/var/nix/profiles/per-user/root
            mkdir -p -m 0700 "$HOME/.nix-defexpr"

            . ${pkgs.nix}/etc/profile.d/nix.sh

            ${pkgs.nix}/bin/nix-env -i ${lib.concatStringsSep " " (with pkgs; [ nix cacert git openssh ])}
          '';
          environmentVariables = {
            ENV = "/etc/profile";
            USER = "root";
            NIX_REMOTE = "daemon";
            PATH = "/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/bin:/sbin:/usr/bin:/usr/sbin";
            NIX_SSL_CERT_FILE = "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt";
          };
          tagList = [ "nix" ];
        };
        docker-images = {
          registrationConfigFile = config.age.secrets."passwords/services/gitlab-runner/registration".path;
          dockerImage = "docker:stable";
          dockerVolumes = [
            "/var/run/docker.sock:/var/run/docker.sock"
          ];
          tagList = [ "docker-images" ];
        };
        default = {
          registrationConfigFile = config.age.secrets."passwords/services/gitlab-runner/registration".path;
          dockerImage = "debian:stable";
        };
      };
    };
    virtualisation.docker = {
      enable = true;
      storageDriver = "zfs";
    };
    age.secrets."passwords/services/gitlab-runner/registration" = {
      file = ../../../secrets/passwords/services/gitlab-runner/registration.age;
    };
  };
}
