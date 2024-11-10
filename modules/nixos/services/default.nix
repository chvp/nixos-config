{ config, pkgs, lib, ... }:

{
  imports = [
    ./accentor
    ./containers
    ./data-access
    ./git
    ./mail
    ./mastodon
    ./matrix
    ./nextcloud
    ./nginx
    ./torrents
  ];

  services.postgresql = {
    package = pkgs.postgresql_15;
    dataDir = "${config.chvp.dataPrefix}/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
  };
}
