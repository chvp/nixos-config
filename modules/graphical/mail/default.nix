{
  config,
  lib,
  pkgs,
  ...
}:

let
  username = config.chvp.username;
  mkThunderbirdAccount =
    {
      address,
      aliases ? [ ],
      primary ? false,
      server ? "mail.vanpetegem.me",
    }:
    {
      inherit address aliases;
      thunderbird.enable = true;
      userName = address;
      realName = "Charlotte Van Petegem";
      primary = lib.mkDefault primary;
      flavor = "plain";
      imap = {
        host = server;
        port = 993;
        tls = {
          enable = true;
          useStartTls = false;
        };
      };
      smtp = {
        host = server;
        port = 465;
        tls = {
          enable = true;
          useStartTls = false;
        };
      };
    };
  mkCaldavAccount = { url }: {
    remote = {
      inherit url;
      type = "caldav";
    };
    thunderbird.enable = true;
  };
in
{
  options.chvp.graphical.mail.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.mail.enable {
    chvp.base.zfs.homeLinks = [
      {
        path = ".cache/thunderbird";
        type = "cache";
      }
      {
        path = ".thunderbird";
        type = "cache";
      }
    ];
    home-manager.users.${username} = {
      accounts = {
        email.accounts = {
          Personal = mkThunderbirdAccount {
            address = "charlotte@vanpetegem.be";
            aliases = [ "charlotte@vanpetegem.me" ];
            primary = true;
          };
          Postbot = mkThunderbirdAccount { address = "postbot@vanpetegem.be"; };
          Posteo = mkThunderbirdAccount {
            address = "chvp@posteo.net";
            server = "posteo.de";
          };
          Webmaster = mkThunderbirdAccount { address = "webmaster@vanpetegem.be"; };
        };
        calendar.accounts = {
          Personal = mkCaldavAccount {
            url = "https://nextcloud.vanpetegem.me/remote.php/dav/calendars/chvp/personal";
          };
          Work = mkCaldavAccount {
            url = "https://nextcloud.vanpetegem.me/remote.php/dav/calendars/chvp/720a1c05-bb33-4926-a449-02a1a3117d94/";
          };
          "Rode Kruis" = mkCaldavAccount {
            url = "https://nextcloud.vanpetegem.me/remote.php/dav/calendars/chvp/medical-1/";
          };
        };
        contact.accounts.Personal = {
          remote = {
            type = "carddav";
            url = "https://nextcloud.vanpetegem.me/remote.php/dav/addressbooks/users/chvp/contacts/";
          };
          thunderbird.enable = true;
        };
      };
    };
    programs.thunderbird.enable = true;
  };
}
