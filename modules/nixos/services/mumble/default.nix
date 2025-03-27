{ config, pkgs, lib, ... }:

{
  options.chvp.services.mumble.enable = lib.mkOption {
    default = false;
    example = true;
  };


  config = lib.mkIf config.chvp.services.mumble.enable {
    services.murmur = {
      enable = true;
      environmentFile = config.age.secrets."passwords/services/murmur".path;
      openFirewall = true;
      password = "$MURMURD_PASSWORD";
      sslKey = "${config.security.acme.certs."vanpetegem.me".directory}/key.pem";
      sslCert = "${config.security.acme.certs."vanpetegem.me".directory}/cert.pem";
      sslCa = "${config.security.acme.certs."vanpetegem.me".directory}/chain.pem";
    };
    users.users.murmur.extraGroups = [ "acme" ];
    age.secrets."passwords/services/murmur" = {
      file = ../../../../secrets/passwords/services/murmur.age;
      owner = "murmur";
    };
  };
}
