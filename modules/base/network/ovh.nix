{ config, lib, ... }:

{
  options.chvp.base.network.ovh = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
    publicIPV4 = lib.mkOption {
      example = {
        ip = "1.2.3.4";
        gateway = "1.2.3.254";
      };
    };
    publicIPV6 = lib.mkOption {
      example = {
        ip = "1:2:3:4::";
        gateway = "1:2:3:ff:ff:ff:ff:ff";
      };
    };
    internalIPV4 = lib.mkOption {
      example = "192.168.0.1";
    };
  };

  config = lib.mkIf config.chvp.base.network.ovh.enable {
    networking.useDHCP = false;
    systemd.network = {
      enable = true;
      networks = with config.chvp.base.network.ovh; {
        eno3 = {
          enable = true;
          matchConfig = { Name = "eno3"; };
          address = [
            "${publicIPV4.ip}/24"
            "${publicIPV6.ip}/64"
          ];
          gateway = [ publicIPV4.gateway ];
          routes = [
            {
              routeConfig = {
                Gateway = publicIPV6.gateway;
                GatewayOnLink = true;
              };
            }
          ];
          dns = [
            "1.1.1.1"
            "1.0.0.1"
            "2606:4700:4700::1111"
            "2606:4700:4700::1001"
          ];
        };
        eno4 = {
          enable = true;
          matchConfig = { Name = "eno4"; };
          address = [ "${internalIPV4}/16" ];
          routes = [
            { routeConfig = { Destination = "${internalIPV4}/16"; }; }
          ];
        };
      };
    };
  };
}
