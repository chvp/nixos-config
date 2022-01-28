{ config, lib, pkgs, ... }:

let
  particles = pkgs.fetchgit {
    url = "https://git.zeus.gent/midgard/particles.git";
    sha256 = "0weSBjhSS0XuII5yZXWYUJpPOhetFJKbcsS8WWpodhs=";
  };
in
{
  options.chvp.games.tetris.server = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.games.tetris.server {
    chvp.services.nginx.hosts = [
      {
        fqdn = "tetris.vanpetegem.me";
        options = {
          root = "${pkgs.tetris}";
          locations."/".index = "index.html";
        };
      }
      {
        fqdn = "particles.vanpetegem.me";
        options = {
          root = "${particles}/public";
          locations."/".index = "index.html";
        };
      }
    ];
  };
}
