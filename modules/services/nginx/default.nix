{ config, lib, ... }:

{
  options.chvp.services.nginx = {
    enable = lib.mkOption {
      readOnly = true;
      default = (builtins.length config.chvp.services.nginx.hosts) > 0;
    };
    hosts = lib.mkOption {
      default = [ ];
      example = [
        {
          fqdn = "data.vanpetegem.me";
          options = {
            default = true;
            root = "/srv/data";
            locations = {
              "/".extraConfig = ''
                autoindex on;
              '';
              "/public".extraConfig = ''
                autoindex on;
                auth_basic off;
              '';
            };
          };
        }
      ];
    };
    extraPostACMEScripts = lib.mkOption {
      default = [ ];
      example = [
        ''
          cp fullchain.pem /data/home/charlotte/synapse/slack/cert.crt
          cp privkey.pem /data/home/charlotte/synapse/slack/key.pem
          pushd /data/home/charlotte/synapse
          ''${pkgs.docker-compose}/bin/docker-compose restart slack
          popd
        ''
      ];
    };
  };

  config = lib.mkIf config.chvp.services.nginx.enable {
    networking.firewall.allowedTCPPorts = [ 80 443 ];
    security.acme = {
      certs."vanpetegem.me" = {
        dnsProvider = "cloudflare";
        credentialsFile = config.age.secrets."passwords/services/acme".path;
        extraDomainNames = [
          "*.vanpetegem.me"
          "cvpetegem.be"
          "*.cvpetegem.be"
          "chvp.be"
          "*.chvp.be"
        ];
        postRun = lib.concatStrings config.chvp.services.nginx.extraPostACMEScripts;
      };
      email = "webmaster@vanpetegem.me";
      acceptTerms = true;
      preliminarySelfsigned = false;
    };
    age.secrets."passwords/services/acme" = {
      file = ../../../secrets/passwords/services/acme.age;
      owner = "acme";
    };
    chvp.base.zfs.systemLinks = [
      { type = "data"; path = "/var/lib/acme"; }
    ];
    services.nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      virtualHosts = builtins.listToAttrs
        (map
          (elem: {
            name = elem.fqdn;
            value = {
              forceSSL = true;
              useACMEHost = "vanpetegem.me";
              locations."/" = lib.mkIf (builtins.hasAttr "basicProxy" elem) {
                proxyPass = elem.basicProxy;
                extraConfig = ''
                  proxy_set_header X-Forwarded-Ssl on;
                '' + (elem.extraProxySettings or "");
              };
            } // (elem.options or { });
          })
          config.chvp.services.nginx.hosts);
    };
    users.users = {
      nginx.extraGroups = [ "acme" ];
      acme.uid = 999;
    };
    users.groups.acme.gid = 999;
  };
}
