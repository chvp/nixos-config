{ config, pkgs, ... }:

{
  imports = [
    ./accentor
    ./containers
    ./data-access
    ./garmin-scraper
    ./git
    ./grafana
    ./mail
    ./mastodon
    ./matrix
    ./nextcloud
    ./nginx
    ./torrents
  ];

  services.postgresql.package = pkgs.postgresql_15;
}
