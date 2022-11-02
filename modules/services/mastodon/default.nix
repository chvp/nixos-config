{ config, lib, pkgs, ... }:

{
  options.chvp.services.mastodon.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.mastodon.enable {
    chvp.services.nginx.hosts = [{
      fqdn = "social.chvp.be";
      options = {
        root = "${pkgs.mastodon}/public/";
        locations = {
          "/system/".alias = "/var/lib/mastodon/public-system/";
          "/".tryFiles = "$uri @proxy";
          "@proxy" = {
            proxyPass = "http://unix:/run/mastodon-web/web.socket";
            proxyWebsockets = true;
          };
          "/api/v1/streaming" = {
            proxyPass = "http://unix:/run/mastodon-streaming/streaming.socket";
            proxyWebsockets = true;
          };
        };
      };
    }];
    users = {
      users = {
        mastodon.uid = 989;
        nginx.extraGroups = [ "mastodon" ];
      };
      groups.mastodon.gid = 985;
    };
    services.mastodon = {
      enable = true;
      configureNginx = false;
      localDomain = "social.chvp.be";
      enableUnixSocket = true;

      database.createLocally = true;
      redis.createLocally = true;
      smtp = {
        fromAddress = "social@chvp.be";
        createLocally = false;
      };
      extraConfig = {
        SMTP_OPENSSL_VERIFY_MODE = "none";
      };

      otpSecretFile = config.age.secrets."passwords/services/mastodon/otp".path;
      secretKeyBaseFile = config.age.secrets."passwords/services/mastodon/key".path;
      vapidPublicKeyFile = config.age.secrets."passwords/services/mastodon/vapid-public".path;
      vapidPrivateKeyFile = config.age.secrets."passwords/services/mastodon/vapid-private".path;
    };

    age.secrets."passwords/services/mastodon/vapid-public" = {
      file = ../../../secrets/passwords/services/mastodon/vapid-public.age;
      owner = "mastodon";
    };
    age.secrets."passwords/services/mastodon/vapid-private" = {
      file = ../../../secrets/passwords/services/mastodon/vapid-private.age;
      owner = "mastodon";
    };
    age.secrets."passwords/services/mastodon/key" = {
      file = ../../../secrets/passwords/services/mastodon/key.age;
      owner = "mastodon";
    };
    age.secrets."passwords/services/mastodon/otp" = {
      file = ../../../secrets/passwords/services/mastodon/otp.age;
      owner = "mastodon";
    };
  };
}
