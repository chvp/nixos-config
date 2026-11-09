{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.chvp.services.jellyfin.enable = lib.mkEnableOption "jellyfin";

  config = lib.mkIf config.chvp.services.jellyfin.enable {
    chvp = {
      base.zfs.systemLinks = [
        {
          path = "/var/cache/jellyfin";
          type = "cache";
        }
      ];
      services.nginx.hosts = [
        {
          fqdn = "jellyfin.vanpetegem.be";
          basicProxy = "http://localhost:8096";
          extraProxyOptions.proxyWebsockets = true;
        }
      ];
    };

    services.jellyfin = {
      enable = true;
      logDir = "/var/log/jellyfin";
    };
  };
}
