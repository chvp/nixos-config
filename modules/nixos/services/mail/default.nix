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
    ];
    mailserver = {
      enable = true;
      stateVersion = 3;
      enableManageSieve = true;
      fqdn = "mail.vanpetegem.me";
      systemName = "vanpetegem.me";
      systemDomain = "mail.vanpetegem.me";
      domains = [
        "accentor.tech"
        "chvp.be"
        "cvpetegem.be"
        "estherdereys.be"
        "eenstweedrie.be"
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
        "peter@vanpetegem.me".hashedPasswordFile = config.age.secrets."passwords/services/mail/peter@vanpetegem.me".path;
        "robbe@vanpetegem.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/robbe@vanpetegem.be".path;
          aliases = [ "robbe.nb@vanpetegem.me" "robbe@vanpetegem.me" ];
        };
        "robbe@robbevanpetegem.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/robbe@robbevanpetegem.be".path;
          aliases = [ "@robbevanpetegem.be" ];
        };
        "hallo@robbe.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/hallo@robbe.be".path;
          aliases = [ "@robbe.be" "@robbevp.be" ];
        };
        "info@eenstweedrie.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/info@eenstweedrie.be".path;
          aliases = [ "@eenstweedrie.be" ];
        };
        "hallo@estherdereys.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/hallo@estherdereys.be".path;
          aliases = [ "@estherdereys.be" ];
        };
        # Service accounts
        "forgejo@robbevp.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/forgejo@robbevp.be".path;
        };
        "git@chvp.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/git@chvp.be".path;
        };
        "noreply@vanpetegem.me" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/noreply@vanpetegem.me".path;
          sendOnly = true;
        };
        "postbot@vanpetegem.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/postbot@vanpetegem.be".path;
          aliases = [ "@vanpetegem.me" "@vanpetegem.be" ];
        };
        "webmaster@vanpetegem.be" = {
          hashedPasswordFile = config.age.secrets."passwords/services/mail/webmaster@vanpetegem.be".path;
          aliases = [ "webmaster@vanpetegem.me" ];
        };
      };
      indexDir = "/var/lib/dovecot/indices";
      fullTextSearch = {
        enable = false;
        memoryLimit = 4000;
      };
      lmtpSaveToDetailMailbox = "no";
      mailboxes = {
        Trash = {
          auto = "no";
          specialUse = "Trash";
          autoexpunge = "60d";
        };
        Junk = {
          auto = "subscribe";
          specialUse = "Junk";
          autoexpunge = "60d";
        };
        Drafts = {
          auto = "subscribe";
          specialUse = "Drafts";
        };
        Sent = {
          auto = "subscribe";
          specialUse = "Sent";
        };
      };
      extraVirtualAliases = {
        "android@accentor.tech" = [ "charlotte@vanpetegem.be" ];
        "team@accentor.tech" = [ "charlotte@vanpetegem.be" "robbe@vanpetegem.be" ];
      };
      forwards = {
        "info@toekomstlabo.be" = "robbe+toekomstlabo@robbevanpetegem.be";
      };
      rejectRecipients = [
        "cindy@vanpetegem.be"
        "contact@vanpetegem.be"
        "info@vanpetegem.be"
        "isabelle@vanpetegem.be"
        "marierose@vanpetegem.be"
        "rudy@vanpetegem.be"
        "sarina@vanpetegem.be"
        "sabrina@vanpetegem.be"
      ];
      rejectSender = [
        "junjunggaming07@gmail.com"
        "censysnetbackup@gmail.com"
        "vitor.carvalheiro@escola.pr.gov.br"
        "spam@vuztc.ru"
        "mofos@theportalnetworks.com" # Someone registered my f-droid contact address on a porn site :(
      ];
      mailDirectory = "/var/vmail";
      useFsLayout = false;
      certificateScheme = "manual";
      certificateFile = certFile;
      keyFile = keyFile;
      dkimKeyDirectory = "/var/dkim";
    };

    services.dovecot2.sieve = {
      extensions = [ "editheader" ];
      scripts.after2 = pkgs.writeText "custom-spam.sieve" ''
        require ["fileinto", "regex"];

        if anyof(
          # Freshdesk is often used to sent spam from emails like `support@info5813.freshdesk.com`
          address :regex "From" "[a-z\d]+@[a-z\d]+\.freshdesk\.com",
          header :contains "From" ["jakubbielec", "Jakub Bielec"],
          # Stop any mail pretending to be from itsme not from their official domains
          allof(
            address :contains "From" "itsme",
            not address :matches :domain "from" ["*itsme.be", "*itsme-id.com"]
          )
        ) {
            fileinto "Junk";
            stop;
        }
      '';
    };

    services.rspamd.extraConfig = ''
      actions {
        reject = null;
        add_header = 5;
        greylist = 4;
      }
    '';

    age.secrets = {
      "passwords/services/mail/charlotte@vanpetegem.be".file = ../../../../secrets/passwords/services/mail/charlotte_at_vanpetegem.be.age;
      "passwords/services/mail/forgejo@robbevp.be".file = ../../../../secrets/passwords/services/mail/forgejo_at_robbevp.be.age;
      "passwords/services/mail/git@chvp.be".file = ../../../../secrets/passwords/services/mail/git_at_chvp.be.age;
      "passwords/services/mail/hallo@estherdereys.be".file = ../../../../secrets/passwords/services/mail/hallo_at_estherdereys.be.age;
      "passwords/services/mail/hallo@robbe.be".file = ../../../../secrets/passwords/services/mail/hallo_at_robbe.be.age;
      "passwords/services/mail/huis@vanpetegem.me".file = ../../../../secrets/passwords/services/mail/huis_at_vanpetegem.me.age;
      "passwords/services/mail/info@eenstweedrie.be".file = ../../../../secrets/passwords/services/mail/info_at_eenstweedrie.be.age;
      "passwords/services/mail/noreply@vanpetegem.me".file = ../../../../secrets/passwords/services/mail/noreply_at_vanpetegem.me.age;
      "passwords/services/mail/peter@vanpetegem.me".file = ../../../../secrets/passwords/services/mail/peter_at_vanpetegem.me.age;
      "passwords/services/mail/postbot@vanpetegem.be".file = ../../../../secrets/passwords/services/mail/postbot_at_vanpetegem.be.age;
      "passwords/services/mail/robbe@robbevanpetegem.be".file = ../../../../secrets/passwords/services/mail/robbe_at_robbevanpetegem.be.age;
      "passwords/services/mail/robbe@vanpetegem.be".file = ../../../../secrets/passwords/services/mail/robbe_at_vanpetegem.be.age;
      "passwords/services/mail/webmaster@vanpetegem.be".file = ../../../../secrets/passwords/services/mail/webmaster_at_vanpetegem.be.age;
    };
  };
}
