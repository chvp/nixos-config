{ config, lib, ... }:

{
  options.chvp.base.network.ovh = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
    publicInterface = lib.mkOption {
      default = "eno3";
      example = "eno1";
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
  };

  config = lib.mkIf config.chvp.base.network.ovh.enable {
    networking.useDHCP = false;
    systemd.network = {
      enable = true;
      networks = with config.chvp.base.network.ovh; {
        "${publicInterface}" = {
          enable = true;
          matchConfig = { Name = "${publicInterface}"; };
          address = [
            "${publicIPV4.ip}/24"
            "${publicIPV6.ip}/64"
          ];
          gateway = [ publicIPV4.gateway ];
          routes = [
            {
              Gateway = publicIPV6.gateway;
              GatewayOnLink = true;
            }
          ];
          dns = [
            "1.1.1.1"
            "1.0.0.1"
            "2606:4700:4700::1111"
            "2606:4700:4700::1001"
          ];
        };
      };
    };
  };
}
