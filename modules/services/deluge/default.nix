{ config, lib, pkgs, ... }:

{
  options.chvp.services.deluge = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
    count = lib.mkOption {
      default = 1;
      example = 6;
    };
  };

  config = lib.mkIf config.chvp.services.deluge.enable {
    chvp.services.nginx.hosts = (builtins.genList
      (n: {
        fqdn = "del${toString (n + 1)}.vanpetegem.me";
        basicProxy = "http://localhost:${toString (8112 + n)}";
      }) config.chvp.services.deluge.count) ++ [
        { fqdn = "transmission.vanpetegem.me"; basicProxy = "http://localhost:9091"; }
      ];

    networking.firewall = {
      allowedTCPPortRanges = [
        { from = 60000; to = 60000 + config.chvp.services.deluge.count - 1; }
        { from = 58846; to = 58846 + config.chvp.services.deluge.count - 1; }
      ];
    };

    services.transmission = {
      enable = true;
      user = "charlotte";
      group = "users";
      home = "/data/var/lib/transmission";
      openRPCPort = true;
      openPeerPorts = true;
      credentialsFile = config.age.secrets."files/programs/transmission/config.json".path;
      settings = {
        umask = 18;
        download-dir = "/srv/data";
        rpc-authentication-required = true;
        rpc-bind-address = "0.0.0.0";
        rpc-enabled = true;
        rpc-host-whitelist-enabled = false;
        rpc-whitelist-enabled = false;
      };
    };

    age.secrets."files/programs/transmission/config.json" = {
      file = ../../../secrets/files/programs/transmission/config.json.age;
      owner = "charlotte";
    };

    systemd.services = builtins.foldl' (x: y: x // y) { } (builtins.genList
      (n:
        let num = toString (n + 1); in
        {
          "del${num}" = {
            after = [ "network-online.target" ];
            requires = [ "network-online.target" ];
            description = "Deluge daemon ${num}";
            wantedBy = [ "multi-user.target" ];
            path = [ pkgs.deluge ];
            serviceConfig = {
              ExecStart = ''
                ${pkgs.deluge}/bin/deluged --do-not-daemonize --config /data/var/lib/deluge/del${toString (n + 1)}
              '';
              Restart = "on-success";
              User = "charlotte";
              Group = "users";
              UMask = "022";
            };
          };
          "del${num}-web" = {
            after = [ "network.target" "del${num}.service" ];
            requires = [ "del${num}.service" ];
            description = "Deluge Web UI for daemon ${num}";
            wantedBy = [ "multi-user.target" ];
            path = [ pkgs.deluge ];
            serviceConfig = {
              ExecStart = ''
                ${pkgs.deluge}/bin/deluge-web --do-not-daemonize --config /data/var/lib/deluge/del${toString (n + 1)} --port ${toString (8112 + n)}
              '';
              User = "charlotte";
              Group = "users";
            };
          };
        })
      config.chvp.services.deluge.count);
  };
}
