{ config, lib, pkgs, ... }:

{
  options.chvp.globalMailer.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.globalMailer.enable {
    services.ssmtp = {
      enable = true;
      authUser = "webmaster@vanpetegem.me";
      authPassFile = config.age.secrets."passwords/services/ssmtp-pass".path;
      domain = "${config.networking.hostName}.vanpetegem.me";
      hostName = "mail.vanpetegem.me:465";
      root = "webmaster@vanpetegem.me";
      setSendmail = true;
      useTLS = true;
    };

    age.secrets."passwords/services/ssmtp-pass".file = ../secrets/passwords/services/ssmtp-pass.age;
  };
}
