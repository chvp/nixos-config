{ config, pkgs, lib, ... }:

{
  imports = [
    ./accentor
    ./containers
    ./data-access
    ./entrance-exam
    ./git
    ./mail
    ./matrix
    ./mumble
    ./nextcloud
    ./nginx
    ./torrents
  ];

  services.postgresql = {
    package = pkgs.postgresql_15;
    dataDir = "/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
  };
}
