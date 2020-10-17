{ pkgs, lib, ... }:
let
  passwordScript = pkgs.writeShellScript "get_mail_password" ''${pkgs.pass}/bin/pass show "$@" | head -n1 | tr -d "\n"'';
  notifyScript = name: pkgs.writeShellScript "notify_${name}_mail" ''
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
        RestartSec = 20;
      };
    };
  };
  toRecursiveINI = with lib.strings; with lib.attrsets; with lib.generators; with lib.lists; let
    repeat = count: char: concatStrings (genList (_: char) count);
    mkHeader = depth: name: concatStrings [ (repeat depth "[") (escape [ "[" ] name) (repeat depth "]") ];
    simpleAttrs = filterAttrs (n: v: !(isAttrs v));
    complexAttrs = filterAttrs (n: v: isAttrs v);
    removeEmpty = filter (v: v != "");
    toRecursiveINIBase = depth: data: (concatStringsSep "\n" (
      mapAttrsToList
        (name: values: concatStringsSep "\n" (removeEmpty [
          (mkHeader depth name)
          (toKeyValue { } (simpleAttrs values))
          (toRecursiveINIBase (depth + 1) (complexAttrs values))
        ]))
        data
    ));
  in
  toRecursiveINIBase 1;
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
        text/html; ${pkgs.qutebrowser}/bin/qutebrowser %s ; nametemplate=%s.html; needsterminal
        text/html; ${pkgs.w3m}/bin/w3m -dump -o display_link_number=1 -o document_charset=%{charset} %s ; copiousoutput; nametemplate=%s.html
      '';
    };
    xdg.configFile = {
      "khal/config".text = toRecursiveINI {
        calendars = {
          calendar = {
            path = "~/.local/share/calendars/*";
            type = "discover";
          };
        };
        locale = {
          timeformat = "%H:%M";
          dateformat = "%Y-%m-%d";
          longdateformat = "%Y-%m-%d";
          datetimeformat = "%Y-%m-%d %H:%M";
          longdatetimeformat = "%Y-%m-%d %H:%M";
        };
      };
      "khard/khard.conf".text = toRecursiveINI {
        addressbooks = {
          contacts = {
            path = "~/.local/share/contacts/contacts";
          };
        };
        general = {
          debug = "no";
          default_action = "list";
          editor = "nvim";
          merge_editor = "nvim, -d";
        };
        "contact table" = {
          display = "formatted_name";
          group_by_addressbook = "no";
          reverse = "no";
          show_nicknames = "no";
          show_uids = "yes";
          sort = "last_name";
          localize_dates = "yes";
          preferred_phone_number_type = "pref, cell, home";
          preferred_email_address_type = "pref, work, home";
        };
        vcard = {
          private_objects = ",";
          preferred_version = "4.0";
          search_in_source_files = "no";
          skip_unparsable = "no";
        };
      };
      "vdirsyncer/config".text =
        let nextcloudConfig = type: {
          inherit type;
          url = "https://nextcloud.vanpetegem.me";
          username = "chvp";
          "password.fetch" = [ "command" "${passwordScript}" "social/Nextcloud" ];
        }; in
        lib.generators.toINI
          { mkKeyValue = lib.generators.mkKeyValueDefault { mkValueString = builtins.toJSON; } "="; }
          {
            general.status_path = "~/.local/share/vdirsyncer";
            "pair nextcloud_contacts" = {
              a = "nextcloud_contacts_local";
              b = "nextcloud_contacts_remote";
              collections = [ "from a" "from b" ];
            };
            "storage nextcloud_contacts_local" = {
              type = "filesystem";
              path = "~/.local/share/contacts";
              fileext = ".vcf";
            };
            "storage nextcloud_contacts_remote" = nextcloudConfig "carddav";
            "pair nextcloud_calendars" = {
              a = "nextcloud_calendars_local";
              b = "nextcloud_calendars_remote";
              collections = [ "from a" "from b" ];
            };
            "storage nextcloud_calendars_local" = {
              type = "filesystem";
              path = "~/.local/share/calendars";
              fileext = ".ics";
            };
            "storage nextcloud_calendars_remote" = nextcloudConfig "caldav";
          };
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
