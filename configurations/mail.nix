{ pkgs, lib, ... }:
let
  passwordScript = pkgs.writeShellScript "get_mail_password" ''${pkgs.pass}/bin/pass show "$@" | head -n1 | tr -d "\n"'';
  notifyScript = name: pkgs.writeShellScript "notify_${name}_mail" ''
    unseen_count=$(${pkgs.mblaze}/bin/mlist -N ~/mail/*/INBOX | wc -l)

    if [ "$unseen_count" = "1" ]
    then
      ${pkgs.libnotify}/bin/notify-send -t 5000 'New ${name} mail arrived' "1 unseen mail"
    elif [ "$unseen_count" != "0" ]
    then
      ${pkgs.libnotify}/bin/notify-send -t 5000 'New ${name} mail arrived' "$unseen_count unseen mails"
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
        onNotify = "${pkgs.isync}/bin/mbsync ${name}:INBOX";
        onNotifyPost = "${pkgs.mu}/bin/mu index && ${notifyScript name}";
      };
      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
        flatten = ".";
        remove = "both";
        extraConfig.account.AuthMechs = "LOGIN";
      };
      msmtp.enable = true;
      mu.enable = true;
      neomutt = {
        enable = true;
        sendMailCommand = "msmtpq --read-envelope-from --read-recipients --account ${name}";
      };
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
  chvp.zfs.homeLinks = [
    { path = "mail"; type = "data"; }
    { path = ".cache/mu"; type = "cache"; }
    { path = ".local/share/contacts"; type = "cache"; }
    { path = ".local/share/calendars"; type = "cache"; }
    { path = ".local/share/vdirsyncer"; type = "cache"; }
  ];
  home-manager.users.charlotte = { ... }: {
    accounts.email = {
      maildirBasePath = "/data/home/charlotte/mail";
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
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "INBOX"; trash = "Trash"; };
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
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "INBOX"; trash = "Deleted Items"; };
            mbsync.extraConfig.account.PipelineDepth = "1";
            neomutt.extraConfig = ''
              alternates dodona@ugent.be
            '';
          };
        };
        work-aap-we-fr = makeAccount {
          name = "work-aap-we-fr";
          address = "aap-we-fr@ugent.be";
          imapHost = "owa.ugent.be";
          smtpHost = "smtp.ugent.be";
          passFile = "work/UGentNet";
          useStartTls = true;
          extraConfig = {
            folders = { drafts = "Concepten"; inbox = "INBOX"; sent = "Verzonden items"; trash = "Verwijderde items"; };
            mbsync.extraConfig.account.PipelineDepth = "1";
            userName = "UGENT\\\\ecvpeteg/aap-we-fr";
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
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "INBOX"; trash = "Trash"; };
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
              sent = "INBOX";
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
            neomutt.extraConfig = ''
              alternates '.*@vanpetegem.me$'
            '';
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "INBOX"; trash = "Trash"; };
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
            folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "INBOX"; trash = "Trash"; };
          };
        };
      };
    };
    home = {
      packages = [ pkgs.khal pkgs.khard ];
      file.".mailcap".text = ''
        text/html; ${pkgs.firefox}/bin/firefox %s ; nametemplate=%s.html; needsterminal
        text/html; ${pkgs.w3m}/bin/w3m -dump -o display_link_number=1 -o document_charset=%{charset} %s ; copiousoutput; nametemplate=%s.html
        text/calendar; ${pkgs.khal}/bin/khal import %s;
        application/pdf; ${pkgs.okular}/bin/okular %s
        image/png; ${pkgs.okular}/bin/okular %s
        image/jpeg; ${pkgs.okular}/bin/okular %s
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
          url = "https://nextcloud.vanpetegem.me/remote.php/dav/";
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
      mbsync.enable = true;
      msmtp.enable = true;
      mu.enable = true;
      neomutt = {
        enable = true;
        sidebar = {
          enable = true;
        };
        extraConfig = ''
          auto_view text/html

          set mail_check_stats
          set query_command = "${pkgs.khard}/bin/khard email -p %s"
          set send_charset="utf-8"
          set print_command = "${pkgs.wl-clipboard}/bin/wl-copy"

          color normal     black      white
          color error      red          white
          color status     white      blue
          color indicator  white      blue
          color quoted     yellow       default
          color header     blue         white "^(Subject)"
          color header     brightblue   white "^(From)"
          color index      white        red     "~D"          # deleted messages
          color attachment green        white
        '';
        macros = [
          {
            map = "index";
            key = "\\Ck";
            action = "<sidebar-prev><sidebar-open>";
          }
          {
            map = "index";
            key = "\\Cj";
            action = "<sidebar-next><sidebar-open>";
          }
        ];
        vimKeys = true;
      };
    };
    services = {
      imapnotify.enable = true;
    };
    systemd.user = {
      services = {
        mbsync = {
          Unit = {
            Description = "MBSync email fetcher";
            After = "network-online.target";
            Wants = "network-online.target";
          };
          Service = {
            Type = "oneshot";
            ExecStart = [ "${pkgs.isync}/bin/mbsync -a" "${pkgs.mu}/bin/mu index" ];
          };
        };
        vdirsyncer = {
          Unit = {
            Description = "VDirSyncer WebDAV syncer";
            After = "network-online.target";
            Wants = "network-online.target";
          };
          Service = {
            Type = "oneshot";
            ExecStart = "${pkgs.vdirsyncer}/bin/vdirsyncer sync";
          };
        };
      };
      timers = {
        mbsync = {
          Unit = { Description = "MBSync email fetcher"; };
          Timer = {
            OnCalendar = "*:0/5";
            Unit = "mbsync.service";
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
