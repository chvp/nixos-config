{ config, lib, pkgs, ... }:

let
  particles = pkgs.fetchgit {
    url = "https://git.zeus.gent/midgard/particles.git";
    hash = "sha256-mwJ5FGFYuvUaUrkRcz8zLCH5bgGXSe7DKqge+Xi5a3I=";
  };
in
{
  options.chvp.games.particles.server = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.games.particles.server {
    chvp.services.nginx.hosts = [{
      fqdn = "particles.vanpetegem.me";
      options = {
        root = "${particles}/public";
        locations."/".index = "index.html";
      };
    }];
  };
}
