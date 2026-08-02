{ config, lib, pkgs, ... }:

{
  options.chvp.games.tetris.server = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.games.tetris.server {
    chvp.services.nginx.hosts = [{
      fqdn = "tetris.vanpetegem.me";
      options = {
        root = "${pkgs.tetris}";
        locations."/".index = "index.html";
      };
    }];
  };
}
