{ config, lib, pkgs, ... }:

{
  options = {
    chvp.games.zeroad = {
      client = lib.mkOption {
        default = false;
        example = true;
      };
      server = lib.mkOption {
        default = false;
        example = true;
      };
    };
  };

  config = lib.mkIf (config.chvp.games.zeroad.server || config.chvp.games.zeroad.client) {
    chvp.base.zfs.homeLinks = [
      { path = ".config/0ad"; type = "cache"; }
    ];

    # Needs to be here, since a headless server probably doesn't have this enabled yet.
    hardware.opengl.enable = true;
    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = [ pkgs.zeroad ];
    };

    networking.firewall = lib.mkIf config.chvp.games.zeroad.server {
      allowedTCPPorts = [ 20595 ];
      allowedUDPPorts = [ 20595 ];
    };
    # Security issues, find suitable alternative
    # services.xrdp = lib.mkIf config.chvp.games.zeroad.server {
    #   enable = true;
    #   defaultWindowManager = "${pkgs.icewm}/bin/icewm";
    # };
  };
}
