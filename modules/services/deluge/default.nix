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
    chvp.services.nginx.hosts = builtins.genList
      (n: {
        fqdn = "del${toString (n + 1)}.vanpetegem.me";
        basicProxy = "http://localhost:${toString (8112 + n)}";
      })
      config.chvp.services.deluge.count;

    networking.firewall = {
      allowedTCPPortRanges = [
        { from = 60000; to = 60000 + config.chvp.services.deluge.count - 1; }
        { from = 58846; to = 58846 + config.chvp.services.deluge.count - 1; }
      ];
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
