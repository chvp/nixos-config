{ pkgs, ... }:
let
  passwordScript = pkgs.writeScript "get_mail_password" ''
    #!${pkgs.bash}/bin/bash

    pass show "$@" | head -n1 | tr -d "\n"
  '';
  baseAccount = {
    gpg = {
      key = "charlotte@vanpetegem.me";
      signByDefault = true;
    };
    msmtp.enable = true;
    offlineimap.enable = true;
    realName = "Charlotte Van Petegem";
    signature = {
      showSignature = "none";
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
        personal = baseAccount // {
          address = "charlotte@vanpetegem.me";
          folders = {
            drafts = "Drafts";
            inbox = "INBOX";
            sent = "Sent";
            trash = "Trash";
          };
          imap = {
            host = "mail.vanpetegem.me";
            port = 993;
            tls.enable = true;
          };
          imapnotify = {
            enable = true;
            boxes = [ "INBOX" ];
            onNotify = "${pkgs.offlineimap}/bin/offlineimap -a personal -f INBOX";
            onNotifyPost = { mail = "${pkgs.libnotify}/bin/notify-send 'New mail arrived'"; };
          };
          passwordCommand = "${passwordScript} mail/Personal";
          primary = true;
          smtp = {
            host = "mail.vanpetegem.me";
            port = 465;
            tls.enable = true;
          };
          userName = "charlotte@vanpetegem.me";
        };
        work = baseAccount // {
          address = "charlotte.vanpetegem@ugent.be";
          folders = {
            drafts = "Drafts";
            inbox = "INBOX";
            sent = "Sent Items";
            trash = "Deleted Items";
          };
          imap = {
            host = "outlook.office365.com";
            port = 993;
            tls.enable = true;
          };
          imapnotify = {
            enable = true;
            boxes = [ "INBOX" ];
            onNotify = "${pkgs.offlineimap}/bin/offlineimap -a work -f INBOX";
            onNotifyPost = { mail = "${pkgs.libnotify}/bin/notify-send 'New mail arrived'"; };
          };
          passwordCommand = "${passwordScript} work/UGentNet";
          smtp = {
            host = "smtp.office365.com";
            port = 587;
            tls = {
              enable = true;
              useStartTls = true;
            };
          };
          userName = "charlotte.vanpetegem@ugent.be";
        };
        posteo = baseAccount // {
          address = "chvp@posteo.net";
          folders = {
            drafts = "Drafts";
            inbox = "INBOX";
            sent = "Sent";
            trash = "Trash";
          };
          imap = {
            host = "posteo.de";
            port = 993;
            tls.enable = true;
          };
          imapnotify = {
            enable = true;
            boxes = [ "INBOX" ];
            onNotify = "${pkgs.offlineimap}/bin/offlineimap -a work -f INBOX";
            onNotifyPost = { mail = "${pkgs.libnotify}/bin/notify-send 'New mail arrived'"; };
          };
          passwordCommand = "${passwordScript} mail/Posteo";
          smtp = {
            host = "posteo.de";
            port = 465;
            tls.enable = true;
          };
          userName = "chvp@posteo.net";
        };
      };
    };
    programs = {
      msmtp.enable = true;
      offlineimap.enable = true;
    };
    services = {
      imapnotify.enable = true;
    };
  };
}
