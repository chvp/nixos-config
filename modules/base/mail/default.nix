{ config, lib, pkgs, ... }:

{
  programs.msmtp = {
    enable = lib.mkDefault true;
    accounts.default = {
      auth = true;
      from = "webmaster@vanpetegem.me";
      host = "mail.vanpetegem.me";
      passwordeval = ''cat ${config.age.secrets."passwords/services/ssmtp-pass".path}'';
      port = 465;
      tls = true;
      tls_starttls = false;
      tls_trust_file = "${pkgs.cacert}/etc/ssl/certs/ca-certificates.crt";
      user = "webmaster@vanpetegem.me";
    };
    setSendmail = true;
  };

  age.secrets."passwords/services/ssmtp-pass".file = ../../../secrets/passwords/services/ssmtp-pass.age;
}
