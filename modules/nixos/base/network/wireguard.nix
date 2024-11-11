{ config, lib, pkgs, ... }:

let
  data = config.chvp.base.network.wireguard.data;
  subnet = config.chvp.base.network.wireguard.subnet;
  pskFile = config.chvp.base.network.wireguard.pskFile;
in
{
  networking.firewall = {
    allowedUDPPorts = lib.optional config.chvp.base.network.wireguard.server 51820;
    allowedTCPPorts = lib.optional config.chvp.base.network.wireguard.server 8080;
    trustedInterfaces = [ "wg0" ];
  };
  boot.kernel.sysctl = lib.mkIf config.chvp.base.network.wireguard.server { "net.ipv4.ip_forward" = 1; };
  services.unbound = lib.mkIf config.chvp.base.network.wireguard.server {
    enable = true;
    resolveLocalQueries = true;
    settings = {
      server = {
        interface = [ "10.240.0.1" "127.0.0.1" "::1" ];
        access-control = [
          "127.0.0.0/8 allow"
          "10.240.0.0/24 allow"
        ];
        private-domain = "internal";
        domain-insecure = "internal";
        local-zone = builtins.map (name: ''"${name}.internal" redirect'') (builtins.attrNames data);
        local-data = builtins.map (name: ''"${name}.internal IN A ${data.${name}.ip}"'') (builtins.attrNames data);
      };
      forward-zone = {
        name = ''"."'';
        forward-addr = [
          "1.1.1.1@853"
          "1.0.0.1@853"
          "2606:4700:4700::1111@853"
          "2606:4700:4700::1001@853"
        ];
        forward-tls-upstream = "yes";
      };
    };
  };
  systemd = {
    network = {
      netdevs.wg0 = {
        enable = true;
        netdevConfig = {
          Name = "wg0";
          Kind = "wireguard";
          MTUBytes = "1342";
        };
        wireguardConfig =
          if config.chvp.base.network.wireguard.server then {
            PrivateKeyFile = data.${config.networking.hostName}.privkeyFile;
            ListenPort = 51820;
          } else {
            PrivateKeyFile = data.${config.networking.hostName}.privkeyFile;
          };
        wireguardPeers =
          if config.chvp.base.network.wireguard.server then
            (builtins.map
              (name: {
                PublicKey = data.${name}.pubkey;
                AllowedIPs = "${data.${name}.ip}/32";
                PresharedKeyFile = pskFile;
              })
              (builtins.filter (name: name != config.networking.hostName) (builtins.attrNames data)))
          else
            ([{
              PublicKey = data.marabethia.pubkey;
              AllowedIPs = subnet;
              Endpoint = "marabethia.vanpetegem.me:51820";
              PresharedKeyFile = pskFile;
              PersistentKeepalive = 25;
            }]);
      };
      networks.wg0 = {
        enable = true;
        name = "wg0";
        address = [ "${data.${config.networking.hostName}.ip}/32" ];
        domains = [ "internal" ];
        dns = [ data.marabethia.ip ];
        linkConfig.MTUBytes = "1342";
        routes = [
          (
            if config.chvp.base.network.wireguard.server then {
              Gateway = "${data.${config.networking.hostName}.ip}";
              Destination = subnet;
            } else {
              Gateway = "${data.marabethia.ip}";
              Destination = subnet;
              GatewayOnLink = true;
            }
          )
        ];
      };
    };
  };
  age.secrets."files/wireguard/psk".owner = "systemd-network";
  age.secrets."files/wireguard/${config.networking.hostName}.privkey".owner = "systemd-network";
}
