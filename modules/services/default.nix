{ config, pkgs, ... }:

{
  imports = [
    ./accentor
    ./containers
    ./data-access
    ./deluge
    ./garmin-scraper
    ./git
    ./grafana
    ./mail
    ./mastodon
    ./matrix
    ./nextcloud
    ./nginx
    ./syncthing
  ];

  services.postgresql.package = pkgs.postgresql_15;
}
