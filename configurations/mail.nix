{ pkgs, lib, ... }:
let
  passwordScript = pkgs.writeScript "get_mail_password" ''
    #!${pkgs.bash}/bin/bash

    ${pkgs.pass}/bin/pass show "$@" | head -n1 | tr -d "\n"
  '';
  notifyScript = name: pkgs.writeScript "notify_${name}_mail" ''
    #!${pkgs.bash}/bin/bash

    unseen_count=$(${pkgs.mblaze}/bin/mlist -N ~/mail/*/INBOX | wc -l)

    if [ "$unseen_count" = "1" ]
    then
      ${pkgs.libnotify}/bin/notify-send -t 5000 'New ${name} mail arrived' \"1 unseen mail\"
    else
      ${pkgs.libnotify}/bin/notify-send -t 5000 'New ${name} mail arrived' \"$unseen_count unseen mails\"
    fi
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
        onNotifyPost = { mail = "${notifyScript name}"; };
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
      Service = {
        Restart = "always";
      };
    };
  };
in
{
  custom.zfs.homeLinks = [
    { path = "mail"; type = "data"; }
    { path = ".local/share/offlineimap"; type = "data"; }
    { path = ".local/share/contacts"; type = "data"; }
    { path = ".local/share/calendars"; type = "data"; }
    { path = ".local/share/vdirsyncer"; type = "data"; }
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
            neomutt.extraConfig = ''
              alternates '^.*@cvpetegem.be$' '^charlotte\+.*@vanpetegem.me'
            '';
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
            neomutt.extraConfig = ''
              alternates dodona@ugent.be
            '';
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "Sent Items"; trash = "Deleted Items"; };
          };
        };
        posteo = makeAccount {
          name = "posteo";
          address = "chvp@posteo.net";
          host = "posteo.de";
          passFile = "mail/Posteo";
          extraConfig = {
            neomutt.extraConfig = ''
              alternates '^chvp\+.*posteo.net'
            '';
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
            neomutt.extraConfig = ''
              alternates it@jonggroen.be rvb@jonggroen.be
            '';
            flavor = "gmail.com";
            folders = {
              drafts = "[Gmail].Drafts";
              inbox = "INBOX";
              sent = "[Gmail].Sent Mail";
              trash = "[Gmail].Bin";
            };
            # IMAPNotify doesn't seem to work for imap.gmail.com.
            imapnotify.enable = false;
          };
        };
        postbot = makeAccount {
          name = "postbot";
          address = "postbot@vanpetegem.me";
          host = "mail.vanpetegem.me";
          passFile = "mail/Postbot";
          extraConfig = {
            neomutt.extraConfig = ''
              alternates '.*@vanpetegem.me$'
            '';
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "Sent"; trash = "Trash"; };
          };
        };
        webmaster = makeAccount {
          name = "webmaster";
          address = "webmaster@vanpetegem.me";
          host = "mail.vanpetegem.me";
          passFile = "mail/Webmaster";
          extraConfig = {
            neomutt.extraConfig = ''
              alternates root@vanpetegem.me
            '';
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "Sent"; trash = "Trash"; };
          };
        };
      };
    };
    home = {
      packages = [ pkgs.khal pkgs.khard ];
      file.".mailcap".text = ''
        text/html; ${pkgs.firefox}/bin/firefox %s ; nametemplate=%s.html; needsterminal
        text/html; ${pkgs.w3m}/bin/w3m -I %{charset} -T text/html ; copiousoutput; nametemplate=%s.html
      '';
    };
    xdg.configFile = {
      "khal/config".text = ''
        [calendars]

        [[calendar]]
        path = ~/.local/share/calendars/*
        type = discover

        [locale]
        timeformat = %H:%M
        dateformat = %Y-%m-%d
        longdateformat = %Y-%m-%d
        datetimeformat = %Y-%m-%d %H:%M
        longdatetimeformat = %Y-%m-%d %H:%M
      '';
      "khard/khard.conf".text = ''
        [addressbooks]
        [[contacts]]
        path = ~/.local/share/contacts/contacts

        [general]
        debug = no
        default_action = list
        # These are either strings or comma seperated lists
        editor = nvim
        merge_editor = nvim, -d

        [contact table]
        # display names by first or last name: first_name / last_name / formatted_name
        display = formatted_name
        # group by address book: yes / no
        group_by_addressbook = no
        # reverse table ordering: yes / no
        reverse = no
        # append nicknames to name column: yes / no
        show_nicknames = no
        # show uid table column: yes / no
        show_uids = yes
        # sort by first or last name: first_name / last_name / formatted_name
        sort = last_name
        # localize dates: yes / no
        localize_dates = yes
        # set a comma separated list of preferred phone number types in descending priority
        # or nothing for non-filtered alphabetical order
        preferred_phone_number_type = pref, cell, home
        # set a comma separated list of preferred email address types in descending priority
        # or nothing for non-filtered alphabetical order
        preferred_email_address_type = pref, work, home

        [vcard]
        # extend contacts with your own private objects
        # these objects are stored with a leading "X-" before the object name in the vcard files
        # every object label may only contain letters, digits and the - character
        # example:
        #   private_objects = Jabber, Skype, Twitter
        # default: ,  (the empty list)
        private_objects = ,
        # preferred vcard version: 3.0 / 4.0
        preferred_version = 4.0
        # Look into source vcf files to speed up search queries: yes / no
        search_in_source_files = no
        # skip unparsable vcard files: yes / no
        skip_unparsable = no
      '';
      "vdirsyncer/config".text = ''
        [general]
        status_path = "~/.local/share/vdirsyncer"

        [pair nextcloud_contacts]
        a = "nextcloud_contacts_local"
        b = "nextcloud_contacts_remote"
        collections = ["from a", "from b"]

        [storage nextcloud_contacts_local]
        type = "filesystem"
        path = "~/.local/share/contacts"
        fileext = ".vcf"

        [storage nextcloud_contacts_remote]
        type = "carddav"
        url = "https://nextcloud.vanpetegem.me/"
        username = "chvp"
        password.fetch = ["command", "${passwordScript}", "social/Nextcloud"]

        [pair nextcloud_calendars]
        a = "nextcloud_calendars_local"
        b = "nextcloud_calendars_remote"
        collections = ["from a", "from b"]

        [storage nextcloud_calendars_local]
        type = "filesystem"
        path = "~/.local/share/calendars"
        fileext = ".ics"

        [storage nextcloud_calendars_remote]
        type = "caldav"
        url = "https://nextcloud.vanpetegem.me/"
        username = "chvp"
        password.fetch = ["command", "${passwordScript}", "social/Nextcloud"]
      '';
    };
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
        vdirsyncer = {
          Unit = {
            Description = "VDirSyncer WebDAV syncer";
            After = "network-online.target";
            Wants = "network-online.target";
          };
          Service = { ExecStart = "${pkgs.vdirsyncer}/bin/vdirsyncer sync"; };
        };
      } // lib.listToAttrs (map genNotifyImapPatch [ "personal" "postbot" "posteo" "webmaster" "work" ]);
      timers = {
        offlineimap = {
          Unit = { Description = "OfflineIMAP email fetcher"; };
          Timer = {
            OnCalendar = "*:0/5";
            Unit = "offlineimap.service";
          };
          Install = { WantedBy = [ "timers.target" ]; };
        };
        vdirsyncer = {
          Unit = { Description = "VDirSyncer WebDAV syncer"; };
          Timer = {
            OnCalendar = "*:0/5";
            Unit = "vdirsyncer.service";
          };
          Install = { WantedBy = [ "timers.target" ]; };
        };
      };
    };
  };
}
