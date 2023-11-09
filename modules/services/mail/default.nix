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
      { path = "/var/lib/opendkim"; type = "cache"; }
      { path = "/var/lib/postfix"; type = "cache"; }
      { path = "/var/lib/redis-rspamd"; type = "cache"; }
      { path = "/var/lib/rspamd"; type = "cache"; }
      { path = "/var/sieve"; type = "data"; }
    ];
    mailserver = {
      enable = true;
      enableManageSieve = true;
      fqdn = "mail.vanpetegem.me";
      domains = [
        "accentor.tech"
        "chvp.be"
        "cvpetegem.be"
        "robbe.be"
        "robbevanpetegem.be"
        "robbevp.be"
        "toekomstlabo.be"
        "vanpetegem.me"
      ];
      localDnsResolver = false;
      loginAccounts = {
        "charlotte@vanpetegem.me" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/charlotte@vanpetegem.me".path;
          aliases = [ "@chvp.be" "@cvpetegem.be" ];
        };
        "huis@vanpetegem.me".hashedPasswordFile = config.age.secrets."passwords/services/mail/huis@vanpetegem.me".path;
        "noreply@vanpetegem.me" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/noreply@vanpetegem.me".path;
          sendOnly = true;
        };
        "peter@vanpetegem.me".hashedPasswordFile = config.age.secrets."passwords/services/mail/peter@vanpetegem.me".path;
        "postbot@vanpetegem.me" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/postbot@vanpetegem.me".path;
          aliases = [ "@vanpetegem.me" ];
        };
        "robbe@vanpetegem.me" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/robbe@vanpetegem.me".path;
          aliases = [ "robbe.nb@vanpetegem.me" ];
        };
        "robbe@robbevanpetegem.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/robbe@robbevanpetegem.be".path;
          aliases = [ "@robbevanpetegem.be" ];
        };
        "hallo@robbe.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/hallo@robbe.be".path;
          aliases = [ "@robbe.be" "@robbevp.be" ];
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
      certificateScheme = "manual";
      certificateFile = certFile;
      keyFile = keyFile;
      dkimKeyDirectory = "${config.chvp.dataPrefix}/var/dkim";
      policydSPFExtraConfig = ''
        whitelist = 40.92.0.0/15,40.107.0.0/16,52.100.0.0/14,104.47.0.0/17,2a01:111:f400::/48,2a01:111:f403::/49,2a01:111:f403:8000::/50,2a01:111:f403:c000::/51,2a01:111:f403:f000::/52
      '';
    };

    services.postfix = {
      config.sender_dependent_default_transport_maps = [ "hash:/etc/postfix/sender_map" ];
      mapFiles.sender_map = pkgs.writeText "postfix-sender-map" ''
        charlotte.vanpetegem@ugent.be smtp:[127.0.0.1]:9797
      '';
    };

    services.rspamd.extraConfig = ''
      actions {
        reject = null;
        add_header = 6;
        greylist = 4;
      }
    '';

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
      "passwords/services/mail/hallo@robbe.be".file = ../../../secrets/passwords/services/mail/hallo_at_robbe.be.age;
      "passwords/services/mail/huis@vanpetegem.me".file = ../../../secrets/passwords/services/mail/huis_at_vanpetegem.me.age;
      "passwords/services/mail/noreply@vanpetegem.me".file = ../../../secrets/passwords/services/mail/noreply_at_vanpetegem.me.age;
      "passwords/services/mail/peter@vanpetegem.me".file = ../../../secrets/passwords/services/mail/peter_at_vanpetegem.me.age;
      "passwords/services/mail/postbot@vanpetegem.me".file = ../../../secrets/passwords/services/mail/postbot_at_vanpetegem.me.age;
      "passwords/services/mail/robbe@robbevanpetegem.be".file = ../../../secrets/passwords/services/mail/robbe_at_robbevanpetegem.be.age;
      "passwords/services/mail/robbe@vanpetegem.me".file = ../../../secrets/passwords/services/mail/robbe_at_vanpetegem.me.age;
      "passwords/services/mail/ugent@cvpetegem.be".file = ../../../secrets/passwords/services/mail/ugent_at_cvpetegem.be.age;
      "passwords/services/mail/webmaster@vanpetegem.me".file = ../../../secrets/passwords/services/mail/webmaster_at_vanpetegem.me.age;
    };
  };
}
