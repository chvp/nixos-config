{ config, lib, pkgs, ... }:

let
  passwordScript = pkgs.writeShellScript "get_mail_password" ''${pkgs.pass}/bin/pass show "$@" | head -n1 | tr -d "\n"'';
  notifyScript = name: pkgs.writeShellScript "notify_${name}_mail" (if config.chvp.graphical then ''
    unseen_count=$(${pkgs.mblaze}/bin/mlist -N ~/mail/*/INBOX | wc -l)

    if [ "$unseen_count" = "1" ]
    then
      ${pkgs.libnotify}/bin/notify-send -t 5000 'New ${name} mail arrived' "1 unseen mail"
    elif [ "$unseen_count" != "0" ]
    then
      ${pkgs.libnotify}/bin/notify-send -t 5000 'New ${name} mail arrived' "$unseen_count unseen mails"
    fi
  '' else ''true'');
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
        onNotifyPost = "${config.chvp.emacs.package}/bin/emacsclient --eval \"(mu4e-update-index)\" && ${notifyScript name}";
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
  options.chvp.mail-client.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.mail-client.enable {
    nixpkgs.overlays = [
      (self: super: {
        khal = super.khal.overrideAttrs (old: { doInstallCheck = false; });
      })
    ];
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
              folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "INBOX"; trash = "Trash"; };
            };
          };
          webmaster = makeAccount {
            name = "webmaster";
            address = "webmaster@vanpetegem.me";
            host = "mail.vanpetegem.me";
            passFile = "mail/Webmaster";
            extraConfig = {
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
            editor = "emacs";
            merge_editor = "${pkgs.writeShellScript "ediff" ''emacs --eval "(ediff-merge-files \"$1\" \"$2\")"''}";
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
              ExecStart = [ "${pkgs.isync}/bin/mbsync -a" "${config.chvp.emacs.package}/bin/emacsclient --eval \"(mu4e-update-index)\"" ];
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
  };
}
