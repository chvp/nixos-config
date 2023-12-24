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
        "vanpetegem.be"
        "vanpetegem.me"
      ];
      localDnsResolver = false;
      loginAccounts = {
        "charlotte@vanpetegem.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/charlotte@vanpetegem.be".path;
          aliases = [ "@chvp.be" "@cvpetegem.be" "charlotte@vanpetegem.me" ];
        };
        "huis@vanpetegem.me".hashedPasswordFile = config.age.secrets."passwords/services/mail/huis@vanpetegem.me".path;
        "noreply@vanpetegem.me" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/noreply@vanpetegem.me".path;
          sendOnly = true;
        };
        "peter@vanpetegem.me".hashedPasswordFile = config.age.secrets."passwords/services/mail/peter@vanpetegem.me".path;
        "postbot@vanpetegem.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/postbot@vanpetegem.be".path;
          aliases = [ "@vanpetegem.me" "@vanpetegem.be" ];
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
        "webmaster@vanpetegem.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/webmaster@vanpetegem.be".path;
          aliases = [ "webmaster@vanpetegem.me" ];
        };
      };
      indexDir = "${config.chvp.cachePrefix}/var/lib/dovecot/indices";
      fullTextSearch = {
        enable = true;
        memoryLimit = 4000;
      };
      lmtpSaveToDetailMailbox = "no";
      extraVirtualAliases = {
        "team@accentor.tech" = [ "charlotte@vanpetegem.be" "robbe@vanpetegem.me" ];
      };
      forwards = {
        "info@toekomstlabo.be" = "robbe+toekomstlabo@robbevanpetegem.be";
      };
      mailDirectory = "${config.chvp.dataPrefix}/var/vmail";
      sieveExtensions = [ "+editheader" ];
      useFsLayout = false;
      certificateScheme = "manual";
      certificateFile = certFile;
      keyFile = keyFile;
      dkimKeyDirectory = "${config.chvp.dataPrefix}/var/dkim";
    };

    services.rspamd.extraConfig = ''
      actions {
        reject = null;
        add_header = 6;
        greylist = 4;
      }
    '';

    age.secrets = {
      "passwords/services/mail/charlotte@vanpetegem.be".file = ../../../secrets/passwords/services/mail/charlotte_at_vanpetegem.be.age;
      "passwords/services/mail/hallo@robbe.be".file = ../../../secrets/passwords/services/mail/hallo_at_robbe.be.age;
      "passwords/services/mail/huis@vanpetegem.me".file = ../../../secrets/passwords/services/mail/huis_at_vanpetegem.me.age;
      "passwords/services/mail/noreply@vanpetegem.me".file = ../../../secrets/passwords/services/mail/noreply_at_vanpetegem.me.age;
      "passwords/services/mail/peter@vanpetegem.me".file = ../../../secrets/passwords/services/mail/peter_at_vanpetegem.me.age;
      "passwords/services/mail/postbot@vanpetegem.be".file = ../../../secrets/passwords/services/mail/postbot_at_vanpetegem.be.age;
      "passwords/services/mail/robbe@robbevanpetegem.be".file = ../../../secrets/passwords/services/mail/robbe_at_robbevanpetegem.be.age;
      "passwords/services/mail/robbe@vanpetegem.me".file = ../../../secrets/passwords/services/mail/robbe_at_vanpetegem.me.age;
      "passwords/services/mail/webmaster@vanpetegem.be".file = ../../../secrets/passwords/services/mail/webmaster_at_vanpetegem.be.age;
    };
  };
}
