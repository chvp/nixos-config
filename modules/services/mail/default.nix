{ config, lib, pkgs, ... }:

let
  keyFile = "${config.security.acme.certs."vanpetegem.me".directory}/key.pem";
  certFile = "${config.security.acme.certs."vanpetegem.me".directory}/fullchain.pem";
in
{
  options.chvp.services.mail.enable = lib.mkEnableOption "mail";

  config = lib.mkIf config.chvp.services.mail.enable {
    chvp.base.zfs.systemLinks = [
      { path = "/var/lib/dhparams"; type = "cache"; }
      { path = "/var/lib/dovecot"; type = "cache"; }
      { path = "/var/lib/knot-resolver"; type = "cache"; }
      { path = "/var/lib/opendkim"; type = "cache"; }
      { path = "/var/lib/postfix"; type = "cache"; }
      { path = "/var/lib/redis-rspamd"; type = "cache"; }
      { path = "/var/lib/rspamd"; type = "cache"; }
    ];
    mailserver = {
      enable = true;
      fqdn = "mail.vanpetegem.me";
      domains = [ "vanpetegem.me" "cvpetegem.be" "chvp.be" "accentor.tech" "toekomstlabo.be" ];
      loginAccounts = {
        "charlotte@vanpetegem.me" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/charlotte@vanpetegem.me".path;
          aliases = [ "@chvp.be" "@cvpetegem.be" ];
        };
        "expenses-noreply@vanpetegem.me" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/expenses-noreply@vanpetegem.me".path;
          sendOnly = true;
        };
        "huis@vanpetegem.me".hashedPasswordFile = config.age.secrets."passwords/services/mail/huis@vanpetegem.me".path;
        "peter@vanpetegem.me".hashedPasswordFile = config.age.secrets."passwords/services/mail/peter@vanpetegem.me".path;
        "postbot@vanpetegem.me" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/postbot@vanpetegem.me".path;
          aliases = [ "@vanpetegem.me" ];
        };
        "robbe@vanpetegem.me" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/robbe@vanpetegem.me".path;
          aliases = [ "robbe.nb@vanpetegem.me" ];
        };
        "ugent@cvpetegem.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/ugent@cvpetegem.be".path;
          aliases = [ "charlotte.vanpetegem@ugent.be" ];
        };
        "webmaster@vanpetegem.me".hashedPasswordFile = config.age.secrets."passwords/services/mail/webmaster@vanpetegem.me".path;
      };
      indexDir = "${config.chvp.cachePrefix}/var/lib/dovecot/indices";
      fullTextSearch = {
        enable = true;
        memoryLimit = 4000;
      };
      lmtpSaveToDetailMailbox = "no";
      extraVirtualAliases = {
        "team@accentor.tech" = [ "charlotte@vanpetegem.me" "robbe@vanpetegem.me" ];
      };
      forwards = {
        "info@toekomstlabo.be" = "robbe+toekomstlabo@robbevanpetegem.be";
      };
      mailDirectory = "${config.chvp.dataPrefix}/var/vmail";
      useFsLayout = false;
      certificateScheme = 1;
      certificateFile = certFile;
      keyFile = keyFile;
      dkimKeyDirectory = "${config.chvp.dataPrefix}/var/dkim";
    };

    services.postfix = {
      config.sender_dependent_default_transport_maps = [ "hash:/etc/postfix/sender_map" ];
      mapFiles.sender_map = pkgs.writeText "postfix-sender-map" ''
        charlotte.vanpetegem@ugent.be smtp:[127.0.0.1]:9797
      '';
    };

    systemd.services.tunnel = {
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      script = "${pkgs.openssh}/bin/ssh -i ${config.age.secrets."files/services/tunnel/key".path} -o ServerAliveInterval=60 -o ExitOnForwardFailure=yes -o ControlPath=none -NT -p $SSH_PORT -L 0.0.0.0:9797:$CONN_HOST:$CONN_PORT $USER@$SSH_HOST";
      serviceConfig = {
        RestartSec = "5s";
        Restart = "on-failure";
        EnvironmentFile = config.age.secrets."files/services/tunnel/env".path;
      };
    };

    age.secrets = {
      "files/services/tunnel/key".file = ../../../secrets/files/services/tunnel/key.age;
      "files/services/tunnel/env".file = ../../../secrets/files/services/tunnel/env.age;
      "passwords/services/mail/charlotte@vanpetegem.me".file = ../../../secrets/passwords/services/mail/charlotte_at_vanpetegem.me.age;
      "passwords/services/mail/expenses-noreply@vanpetegem.me".file = ../../../secrets/passwords/services/mail/expenses-noreply_at_vanpetegem.me.age;
      "passwords/services/mail/huis@vanpetegem.me".file = ../../../secrets/passwords/services/mail/huis_at_vanpetegem.me.age;
      "passwords/services/mail/peter@vanpetegem.me".file = ../../../secrets/passwords/services/mail/peter_at_vanpetegem.me.age;
      "passwords/services/mail/postbot@vanpetegem.me".file = ../../../secrets/passwords/services/mail/postbot_at_vanpetegem.me.age;
      "passwords/services/mail/robbe@vanpetegem.me".file = ../../../secrets/passwords/services/mail/robbe_at_vanpetegem.me.age;
      "passwords/services/mail/ugent@cvpetegem.be".file = ../../../secrets/passwords/services/mail/ugent_at_cvpetegem.be.age;
      "passwords/services/mail/webmaster@vanpetegem.me".file = ../../../secrets/passwords/services/mail/webmaster_at_vanpetegem.me.age;
    };
  };
}
