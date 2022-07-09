{ config, lib, pkgs, ... }:

let
  passwordScript = pkgs.writeShellScript "get_mail_password" ''${pkgs.pass}/bin/pass show "$@" | ${pkgs.coreutils}/bin/head -n1 | ${pkgs.coreutils}/bin/tr -d "\n"'';
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
        onNotifyPost = "${config.chvp.base.emacs.package}/bin/emacsclient --eval \"(mu4e-update-index)\" && ${notifyScript name}";
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
  options.chvp.graphical.mail.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.mail.enable {
    nixpkgs.overlays = [
      (self: super: {
        khal = super.khal.overrideAttrs (old: { doInstallCheck = false; });
      })
    ];
    chvp = {
      base = {
        emacs.extraConfig =
          let
            mkAccountConfig = account: ''
              (make-mu4e-context
                :name "${account.name}"
                :match-func (lambda (msg) (when msg (string-prefix-p "/${account.maildir.path}/" (mu4e-message-field msg :maildir))))
                :vars '(
                        (user-mail-address . "${account.address}")
                        (user-full-name . "${account.realName}")
                        (mu4e-drafts-folder . "/${account.maildir.path}/${account.folders.drafts}")
                        (mu4e-sent-folder . "/${account.maildir.path}/${account.folders.sent}")
                        (mu4e-refile-folder . "/${account.maildir.path}/${account.folders.trash}")
                        (mu4e-trash-folder . "/${account.maildir.path}/${account.folders.trash}")
                        (message-sendmail-extra-arguments . ("--read-envelope-from" "--account" "${account.name}"))
                        )
               )
            '';
            hmConfig = config.home-manager.users.charlotte;
          in
          [
            ''
              (use-package mu4e
                ;; Use mu4e included in the mu package, see emacs/default.nix
                :ensure nil
                :commands (mu4e mu4e-update-index)
                :after (selectrum)
                :hook
                (mu4e-view-mode . display-line-numbers-mode)
                (mu4e-view-mode . visual-line-mode)
                (mu4e-compose-mode . mail/auto-dodona-cc-reply-to)
                (mu4e-compose-mode . visual-line-mode)
                (mu4e-compose-mode . (lambda () (setq use-hard-newlines nil)))
                :custom
                (mu4e-change-filenames-when-moving t "Avoid sync issues with mbsync")
                (mu4e-maildir "${hmConfig.accounts.email.maildirBasePath}" "Root of the maildir hierarchy")
                (mu4e-context-policy 'pick-first "Use the first mail context in the list")
                (mu4e-attachment-dir "/home/charlotte/downloads/" "Save attachments to downloads folder")
                (mu4e-compose-dont-reply-to-self t "Don't reply to myself on reply to all")
                (mu4e-compose-format-flowed t "Send format=flowed mails when use-hard-newlines gets enabled")
                (fill-flowed-display-column 1000000000000 "Dont fill when decoding flowed messages, let visual-line-mode handle it")
                (gnus-treat-fill-long-lines nil "Let visual-line-mode handle filling")
                (mu4e-confirm-quit nil "Don't confirm when quitting")
                (mu4e-completing-read-function 'completing-read "Use default completing read function")
                (mu4e-headers-include-related nil "Don't show related messages by default")
                (mu4e-headers-skip-duplicates nil "Show duplicate emails")
                (message-kill-buffer-on-exit t "Close buffer when finished with email")
                (mm-verify-option 'known "Always verify PGP signatures (known protocols)")
                (mm-discouraged-alternatives '("text/html" "text/richtext") "Discourage showing HTML views")
                (gnus-buttonized-mime-types '("multipart/signed") "Make sure signature verification is always shown")
                (mml-secure-openpgp-sign-with-sender t "Sign mails with the sender")
                (sendmail-program "msmtp" "Use msmtp to send email")
                (message-sendmail-f-is-evil t "Remove username from the emacs message")
                (message-send-mail-function 'message-send-mail-with-sendmail "Use sendmail to send mail instead internal smtp")
                (message-cite-reply-position 'below "Bottom posting is the correct way to reply to email")
                :config
                (setq mu4e-contexts (list ${lib.concatStringsSep "\n" (map mkAccountConfig (lib.attrValues hmConfig.accounts.email.accounts))}))
                (add-to-list
                 'mu4e-bookmarks
                  '(:name "Combined inbox" :query "maildir:/personal/INBOX or maildir:/work/INBOX or maildir:/posteo/INBOX" :key ?i)
                 )
                (define-advice mu4e--context-ask-user
                    (:around (orig-fun &rest args) mu4e~context-ask-user-completing-read)
                  "Replace `mu4e-read-option` by general-purpose completing-read"
                  (cl-letf (((symbol-function 'mu4e-read-option)
                             (lambda (prompt options)
                               (let* ((prompt (mu4e-format "%s" prompt))
                                      (choice (completing-read prompt (cl-mapcar #'car options) nil t))
                                      (chosen-el (cl-find-if (lambda (option) (equal choice (car option))) options)))
                                 (if chosen-el
                                     (cdr chosen-el)
                                   (mu4e-warn "Unknown option: '%s'" choice))))))
                    (apply orig-fun args)))
                (define-skeleton mail/dodona-teacher-reply-skeleton
                  "Inserts a typical reply when someone uses the general form for a Dodona teacher request."
                  "Naam leerkracht: "
                  "Dag " str ",\n"
                  "\n"
                  _
                  "\n"
                  "Welkom op Dodona! Zou je het volgende formulier kunnen invullen?\n"
                  "\n"
                  "https://dodona.ugent.be/rights_requests/new/\n"
                  "\n"
                  "Zo hebben we meteen alle info die we nodig hebben om je "
                  "lesgeversrechten te geven op Dodona.\n"
                  "\n"
                  "Met vriendelijke groeten,\n"
                  "Charlotte Van Petegem"
                  )
                (defun mail/dodona-cc-reply-to ()
                  "Add dodona@ugent.be in cc and reply-to headers."
                  (interactive)
                  (save-excursion (message-add-header "Cc: dodona@ugent.be\nReply-To: dodona@ugent.be\n"))
                  )
                (defun mail/auto-dodona-cc-reply-to ()
                  "Set dodona@ugent.be in CC and Reply-To headers when message was directed to dodona@ugent.be"
                  (let ((msg mu4e-compose-parent-message))
                    (when (and msg (mu4e-message-contact-field-matches msg :to "dodona@ugent.be")) (mail/dodona-cc-reply-to))
                    )
                  )
                :general
                (lmap "m" '(mu4e :which-key "mail"))
                ;; Unmap SPC in the mail view so we can still use the leader.
                (lmap mu4e-view-mode-map "" nil)
                (lmap mu4e-compose-mode-map
                  "SPC s" '(mml-secure-message-sign-pgpmime :which-key "Sign")
                  "SPC c" '(mml-secure-message-encrypt-pgpmime :which-key "Encrypt")
                  "SPC t" '(mail/dodona-teacher-reply-skeleton :which-key "Teacher rights reply")
                  "SPC d" '(mail/dodona-cc-reply-to :which-key "Dodona support headers")
                  "SPC f" '(mu4e-toggle-use-hard-newlines :which-key "Toggle format=flowed/hard newlines")
                  )
                )

              (use-package visual-fill-column
                :custom (visual-fill-column-enable-sensible-window-split t "Sensibly split windows in visual-fill-column-mode")
                :hook (visual-line-mode . visual-fill-column-mode)
              )

              (use-package adaptive-wrap
                :hook (visual-fill-column-mode . adaptive-wrap-prefix-mode)
              )
            ''
          ];
        zfs.homeLinks = [
          { path = "mail"; type = "data"; }
          { path = ".cache/mu"; type = "cache"; }
          { path = ".local/share/contacts"; type = "cache"; }
          { path = ".local/share/calendars"; type = "cache"; }
          { path = ".local/share/vdirsyncer"; type = "cache"; }
        ];
      };
    };
    home-manager.users.charlotte = { ... }: {
      accounts.email = {
        maildirBasePath = "/home/charlotte/mail";
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
            host = "mail.vanpetegem.me";
            passFile = "work/UGentNet";
            useStartTls = true;
            extraConfig = {
              folders = { drafts = "Drafts"; inbox = "INBOX"; sent = "INBOX"; trash = "Trash"; };
              userName = "ugent@cvpetegem.be";
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
      home.packages = [ pkgs.khal pkgs.khard ];
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
              ExecStart = [ "${pkgs.isync}/bin/mbsync -a" "${config.chvp.base.emacs.package}/bin/emacsclient --eval \"(mu4e-update-index)\"" ];
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
