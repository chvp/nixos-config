{ config, pkgs, lib, ... }:

{
  imports = [
    ./accentor
    ./attic
    ./containers
    ./data-access
    ./git
    ./mail
    ./matrix
    ./mumble
    ./nextcloud
    ./nginx
    ./torrents
  ];

  services.postgresql = {
    package = pkgs.postgresql_16;
    dataDir = "/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
  };
}
