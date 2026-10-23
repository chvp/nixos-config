{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.chvp.services.sso.enable = lib.mkEnableOption "SSO";

  config = lib.mkIf config.chvp.services.sso.enable {
    chvp.services.nginx.hosts = [
      {
        fqdn = "sso.vanpetegem.be";
        basicProxy = "https://${config.services.kanidm.server.settings.bindaddress}";
      }
    ];

    services.kanidm = {
      package = pkgs.kanidm_1_11;
      server = {
        enable = true;
        settings = {
          domain = "sso.vanpetegem.be";
          origin = "https://sso.vanpetegem.be";
          bindaddress = "[::1]:1443";
          ldapbindaddress = "[::]:636";
          db_fs_type = "zfs";
          http_client_address_info.x-forward-for = [ "::1" ];
          tls_chain = "/var/lib/kanidm/cert.pem";
          tls_key = "/var/lib/kanidm/key.pem";
        };
      };
      client = {
        enable = true;
        settings.uri = "https://sso.vanpetegem.be";
      };
    };

    security.acme.certs."vanpetegem.me" = {
      postRun = ''
        cp -Lv {cert,key,chain}.pem /var/lib/kanidm/
        chown kanidm:kanidm /var/lib/kanidm/{cert,key,chain}.pem
        chmod 400 /var/lib/kanidm/{cert,key,chain}.pem
      '';
      reloadServices = [ "kanidm.service" ];
    };
  };
}
