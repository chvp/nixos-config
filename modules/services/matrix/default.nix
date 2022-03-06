{ config, lib, pkgs, ... }:

{
  options.chvp.services.matrix.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.matrix.enable {
    chvp.services.nginx.hosts = [{
      fqdn = "matrix.vanpetegem.me";
      options.locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:8448";
          extraConfig = ''
            proxy_set_header X-Forwarded-Ssl on;
            proxy_read_timeout 600;
            client_max_body_size 10M;
          '';
        };
        "/_slack" = {
          proxyPass = "http://127.0.0.1:9898";
          extraConfig = ''
            proxy_set_header X-Forwarded-Ssl on;
          '';
        };
      };
    }];

    services = {
      matrix-synapse = {
        enable = true;
        server_name = "vanpetegem.me";
        public_baseurl = "https://matrix.vanpetegem.me";
        listeners = [{
          port = 8448;
          bind_address = "localhost";
          type = "http";
          tls = false;
          x_forwarded = true;
          resources = [
            { names = [ "client" ]; compress = true; }
            { names = [ "federation" ]; compress = false; }
          ];
        }];
        url_preview_enabled = true;
        enable_metrics = false;
        enable_registration = false;
        report_stats = false;
        allow_guest_access = false;
        app_service_config_files = [
          config.age.secrets."files/services/matrix-synapse/whatsapp-registration.yml".path
          config.age.secrets."files/services/matrix-synapse/slack-registration.yml".path
        ];
        extraConfigFiles = [
          config.age.secrets."files/services/matrix-synapse/config.yml".path
        ];
        dataDir = "${config.chvp.dataPrefix}/var/lib/matrix-synapse";
      };
      postgresql = {
        enable = true;
        dataDir = "${config.chvp.dataPrefix}/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
        ensureDatabases = [
          "matrix-synapse"
          "matrix_appservice_slack"
          "mautrix_whatsapp"
        ];
        ensureUsers = [
          {
            name = "matrix_appservice_slack";
            ensurePermissions = {
              "DATABASE matrix_appservice_slack" = "ALL PRIVILEGES";
            };
          }
          {
            name = "mautrix_whatsapp";
            ensurePermissions = {
              "DATABASE mautrix_whatsapp" = "ALL PRIVILEGES";
            };
          }
          {
            name = "matrix-synapse";
            ensurePermissions = {
              "DATABASE \"matrix-synapse\"" = "ALL PRIVILEGES";
            };
          }
        ];
      };
    };

    systemd.services = {
      matrix-appservice-slack = {
        description = "Matrix <-> Slack bridge";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "postgresql.service" "matrix-synapse.service" ];
        requires = [ "postgresql.service" "matrix-synapse.service" ];
        script = "${pkgs.matrix-appservice-slack}/bin/matrix-appservice-slack --config ${config.age.secrets."files/services/matrix-appservice-slack/config.yml".path} --file ${config.age.secrets."files/services/matrix-appservice-slack/registration.yml".path}";
        serviceConfig = {
          User = "matrix_appservice_slack";
          Group = "matrix_appservice_slack";
        };
      };
      matrix-synapse = {
        requires = [ "postgresql.service" ];
      };
      mautrix-whatsapp = {
        description = "Matrix <-> WhatsApp bridge";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "postgresql.service" "matrix-synapse.service" ];
        requires = [ "postgresql.service" "matrix-synapse.service" ];
        script = "${pkgs.mautrix-whatsapp}/bin/mautrix-whatsapp --config ${config.age.secrets."files/services/mautrix-whatsapp/config.yml".path}";
        serviceConfig = {
          User = "mautrix_whatsapp";
          Group = "mautrix_whatsapp";
        };
      };
    };
    systemd.tmpfiles.rules = [
      "d /var/log/mautrix-whatsapp - mautrix_whatsapp mautrix_whatsapp"
    ];

    users = {
      users = {
        matrix_appservice_slack = {
          uid = 998;
          group = "matrix_appservice_slack";
          isSystemUser = true;
        };
        mautrix_whatsapp = {
          uid = 997;
          group = "mautrix_whatsapp";
          isSystemUser = true;
        };
      };
      groups = {
        matrix_appservice_slack = {
          gid = 998;
        };
        mautrix_whatsapp = {
          gid = 997;
        };
      };
    };

    age.secrets."files/services/matrix-appservice-slack/config.yml" = {
      file = ../../../secrets/files/services/matrix-appservice-slack/config.yml.age;
      owner = "matrix_appservice_slack";
    };
    age.secrets."files/services/matrix-appservice-slack/registration.yml" = {
      file = ../../../secrets/files/services/matrix-appservice-slack/registration.yml.age;
      owner = "matrix_appservice_slack";
    };
    age.secrets."files/services/mautrix-whatsapp/config.yml" = {
      file = ../../../secrets/files/services/mautrix-whatsapp/config.yml.age;
      owner = "mautrix_whatsapp";
    };
    age.secrets."files/services/mautrix-whatsapp/registration.yml" = {
      file = ../../../secrets/files/services/mautrix-whatsapp/registration.yml.age;
      owner = "mautrix_whatsapp";
    };
    age.secrets."files/services/matrix-synapse/config.yml" = {
      file = ../../../secrets/files/services/matrix-synapse/config.yml.age;
      owner = "matrix-synapse";
    };
    age.secrets."files/services/matrix-synapse/slack-registration.yml" = {
      file = ../../../secrets/files/services/matrix-appservice-slack/registration.yml.age;
      owner = "matrix-synapse";
    };
    age.secrets."files/services/matrix-synapse/whatsapp-registration.yml" = {
      file = ../../../secrets/files/services/mautrix-whatsapp/registration.yml.age;
      owner = "matrix-synapse";
    };
  };
}
