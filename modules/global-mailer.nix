{ config, lib, pkgs, ... }:

{
  options.chvp.globalMailer.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.smartd.enable {
    services.ssmtp = {
      enable = true;
      authUser = "webmaster@vanpetegem.me";
      authPassFile = "/data/var/secrets/ssmtp-mail-pass";
      domain = "${config.networking.hostName}.vanpetegem.me";
      hostName = "mail.vanpetegem.me:465";
      root = "webmaster@vanpetegem.me";
      setSendmail = true;
      useTLS = true;
    };
  };
}
