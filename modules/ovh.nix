{ config, lib, ... }:

{
  options.chvp.ovh = {
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

  config = lib.mkIf config.chvp.ovh.enable {
    networking = with config.chvp.ovh; {
      useDHCP = false;
      interfaces = {
        eno1.useDHCP = false;
        eno2.useDHCP = false;
        eno3 = {
          useDHCP = false;
          ipv4.addresses = [{
            address = publicIPV4.ip;
            prefixLength = 24;
          }];
          ipv6 = {
            addresses = [{
              address = publicIPV6.ip;
              prefixLength = 64;
            }];
            routes = [{
              address = publicIPV6.gateway;
              prefixLength = 128;
            }];
          };
        };
        eno4 = {
          useDHCP = false;
          ipv4.addresses = [{
            address = internalIPV4;
            prefixLength = 16;
          }];
        };
      };
      defaultGateway = publicIPV4.gateway;
      defaultGateway6 = publicIPV6.gateway;
      nameservers = [ "1.1.1.1" "1.0.0.1" "2606:4700:4700::1111" "2606:4700:4700::1001" ];
    };
  };
}
