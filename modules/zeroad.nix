{ config, lib, pkgs, ... }:

{
  options = {
    chvp.zeroad = {
      enable = lib.mkOption {
        default = false;
        example = true;
      };
      asServer = lib.mkOption {
        default = false;
        example = true;
      };
    };
  };

  config = lib.mkIf config.chvp.zeroad.enable {
    chvp.zfs.homeLinks = [
      { path = ".config/0ad"; type = "cache"; }
    ];

    hardware.opengl.enable = true;
    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = [ pkgs.zeroad ];
    };

    networking.firewall = lib.mkIf config.chvp.zeroad.asServer {
      allowedTCPPorts = [ 20595 ];
      allowedUDPPorts = [ 20595 ];
    };
    services.xrdp = lib.mkIf config.chvp.zeroad.asServer {
      enable = true;
      defaultWindowManager = "${pkgs.icewm}/bin/icewm";
    };
  };
}
