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

    nixpkgs.config.permittedInsecurePackages = [
      "spidermonkey-38.8.0"
    ];

    hardware.opengl.enable = true;
    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = [ pkgs.zeroad ];
    };
    networking.firewall = lib.mkIf config.chvp.zeroad.asServer {
      allowedTCPPorts = [ 20595 ];
      allowedUDPPorts = [ 20595 ];
    };
    services.openssh.forwardX11 = lib.mkDefault config.chvp.zeroad.asServer;
  };
}
