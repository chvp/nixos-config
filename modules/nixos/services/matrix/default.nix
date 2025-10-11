{ config, lib, pkgs, ... }:

{
  options.chvp.services.matrix.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.matrix.enable {
    chvp.services.nginx.hosts = [
      {
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
        };
      }
    ];

    services = {
      matrix-synapse = {
        enable = true;
        settings = {
          server_name = "vanpetegem.me";
          public_baseurl = "https://matrix.vanpetegem.me";
          listeners = [{
            port = 8448;
            bind_addresses = [ "::1" "127.0.0.1" ];
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
          forgotten_room_retention_period = "28d";
          report_stats = false;
          allow_guest_access = false;
          suppress_key_server_warning = true;
          app_service_config_files = [
            config.age.secrets."files/services/matrix-synapse/whatsapp-registration.yml".path
          ];
        };
        extraConfigFiles = [
          config.age.secrets."files/services/matrix-synapse/config.yml".path
        ];
        dataDir = "/var/lib/matrix-synapse";
      };
      postgresql = {
        enable = true;
        ensureDatabases = [
          "matrix-synapse"
          "mautrix-whatsapp"
        ];
        ensureUsers = [
          {
            name = "mautrix-whatsapp";
            ensureDBOwnership = true;
          }
          {
            name = "matrix-synapse";
            ensureDBOwnership = true;
          }
        ];
      };
    };

    systemd.services = {
      matrix-synapse = {
        after = [ "postgresql.service" ];
        requires = [ "postgresql.service" ];
        serviceConfig = {
          RestartMode = "direct";
        };
      };
      mautrix-whatsapp = {
        description = "Matrix <-> WhatsApp bridge";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "postgresql.service" "matrix-synapse.service" ];
        requires = [ "postgresql.service" "matrix-synapse.service" ];
        upholds = [ "matrix-synapse.service" ];
        script = "${pkgs.mautrix-whatsapp}/bin/mautrix-whatsapp --config ${config.age.secrets."files/services/mautrix-whatsapp/config.yml".path}";
        serviceConfig = {
          User = "mautrix-whatsapp";
          Group = "mautrix-whatsapp";
        };
      };
    };

    users = {
      users.mautrix-whatsapp = {
        group = "mautrix-whatsapp";
        home = "/var/lib/mautrix-whatsapp";
        createHome = true;
        isSystemUser = true;
      };
      groups.mautrix-whatsapp = { };
    };

    age.secrets."files/services/mautrix-whatsapp/config.yml" = {
      file = ../../../../secrets/files/services/mautrix-whatsapp/config.yml.age;
      owner = "mautrix-whatsapp";
    };
    age.secrets."files/services/mautrix-whatsapp/registration.yml" = {
      file = ../../../../secrets/files/services/mautrix-whatsapp/registration.yml.age;
      owner = "mautrix-whatsapp";
    };
    age.secrets."files/services/matrix-synapse/config.yml" = {
      file = ../../../../secrets/files/services/matrix-synapse/config.yml.age;
      owner = "matrix-synapse";
    };
    age.secrets."files/services/matrix-synapse/whatsapp-registration.yml" = {
      file = ../../../../secrets/files/services/mautrix-whatsapp/registration.yml.age;
      owner = "matrix-synapse";
    };
  };
}
