{ pkgs, lib, ... }:
let
  passwordScript = pkgs.writeScript "get_mail_password" ''
    #!${pkgs.bash}/bin/bash

    ${pkgs.pass}/bin/pass show "$@" | head -n1 | tr -d "\n"
  '';
  makeAccount = { name, address, host ? "", imapHost ? host, smtpHost ? host, useStartTls ? false, passFile, extraConfig ? { } }: (lib.recursiveUpdate
    {
      inherit address;
      gpg = {
        key = "charlotte@vanpetegem.me";
        signByDefault = true;
      };
      imap = {
        host = imapHost;
        port = 993;
        tls.enable = true;
      };
      imapnotify = {
        enable = true;
        boxes = [ "INBOX" ];
        onNotify = "${pkgs.offlineimap}/bin/offlineimap -a ${name} -f INBOX";
        onNotifyPost = { mail = "${pkgs.libnotify}/bin/notify-send 'New ${name} mail arrived'"; };
      };
      msmtp.enable = true;
      neomutt = {
        enable = true;
        sendMailCommand = "msmtpq --read-envelope-from --read-recipients --account ${name}";
      };
      offlineimap.enable = true;
      passwordCommand = "${passwordScript} ${passFile}";
      realName = "Charlotte Van Petegem";
      signature = {
        showSignature = "none";
      };
      smtp = {
        host = smtpHost;
        port = if useStartTls then 587 else 465;
        tls = {
          enable = true;
          inherit useStartTls;
        };
      };
      userName = address;
    }
    extraConfig);
  genNotifyImapPatch = account: {
    name = "imapnotify-${account}";
    value = {
      Unit = {
        After = "network-online.target";
        Wants = "network-online.target";
      };
    };
  };
in
{
  custom.zfs.homeLinks = [
    { path = "mail"; type = "data"; }
    { path = ".local/share/offlineimap"; type = "data"; }
  ];
  home-manager.users.charlotte = { ... }: {
    accounts.email = {
      maildirBasePath = "mail";
      accounts = {
        personal = makeAccount {
          name = "personal";
          address = "charlotte@vanpetegem.me";
          host = "mail.vanpetegem.me";
          passFile = "mail/Personal";
          extraConfig = {
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "Sent"; trash = "Trash"; };
            primary = true;
          };
        };
        work = makeAccount {
          name = "work";
          address = "charlotte.vanpetegem@ugent.be";
          imapHost = "outlook.office365.com";
          smtpHost = "smtp.office365.com";
          passFile = "work/UGentNet";
          useStartTls = true;
          extraConfig = {
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "Sent Items"; trash = "Deleted Items"; };
          };
        };
        posteo = makeAccount {
          name = "posteo";
          address = "chvp@posteo.net";
          host = "posteo.de";
          passFile = "mail/Posteo";
          extraConfig = {
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "Sent"; trash = "Trash"; };
          };
        };
        jonggroen = makeAccount {
          name = "jonggroen";
          address = "charlotte@jonggroen.be";
          imapHost = "imap.gmail.com";
          smtpHost = "smtp.gmail.com";
          passFile = "jonggroen/GoogleAppMail";
          useStartTls = true;
          extraConfig = {
            flavor = "gmail.com";
            folders = {
              drafts = "[Gmail].Drafts";
              inbox = "INBOX";
              sent = "[Gmail].Sent Mail";
              trash = "[Gmail].Bin";
            };
          };
        };
        postbot = makeAccount {
          name = "postbot";
          address = "postbot@vanpetegem.me";
          host = "mail.vanpetegem.me";
          passFile = "mail/Postbot";
          extraConfig = {
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "Sent"; trash = "Trash"; };
          };
        };
        webmaster = makeAccount {
          name = "webmaster";
          address = "webmaster@vanpetegem.me";
          host = "mail.vanpetegem.me";
          passFile = "mail/Webmaster";
          extraConfig = {
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "Sent"; trash = "Trash"; };
          };
        };
      };
    };
    home.file.".mailcap".text = ''
      text/html; ${pkgs.firefox}/bin/firefox %s ; nametemplate=%s.html; needsterminal
      text/html; ${pkgs.w3m}/bin/w3m -I %{charset} -T text/html ; copiousoutput; nametemplate=%s.html
    '';
    programs = {
      msmtp.enable = true;
      neomutt = {
        enable = true;
        sidebar = {
          enable = true;
        };
        extraConfig = ''
          auto_view text/html
        '';
        vimKeys = true;
      };
      offlineimap.enable = true;
    };
    services = {
      imapnotify.enable = true;
    };
    systemd.user = {
      services = {
        offlineimap = {
          Unit = {
            Description = "OfflineIMAP email fetcher";
            After = "network-online.target";
            Wants = "network-online.target";
          };
          Service = { ExecStart = "${pkgs.offlineimap}/bin/offlineimap"; };
        };
      } // lib.listToAttrs (map genNotifyImapPatch [ "jonggroen" "personal" "postbot" "posteo" "webmaster" "work" ]);
      timers = {
        offlineimap = {
          Unit = { Description = "OfflineIMAP email fetcher"; };
          Timer = {
            OnCalendar = "*:0/5";
            Unit = "offlineimap.service";
          };
          Install = { WantedBy = [ "timers.target" ]; };
        };
      };
    };
  };
}
