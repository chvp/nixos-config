{ config, lib, pkgs, ... }:

{
  services.ssmtp = {
    enable = lib.mkDefault true;
    authUser = "webmaster@vanpetegem.me";
    authPassFile = config.age.secrets."passwords/services/ssmtp-pass".path;
    domain = "${config.networking.hostName}.vanpetegem.me";
    hostName = "mail.vanpetegem.me:465";
    root = "webmaster@vanpetegem.me";
    setSendmail = true;
    useTLS = true;
  };

  age.secrets."passwords/services/ssmtp-pass".file = ../../../secrets/passwords/services/ssmtp-pass.age;
}
