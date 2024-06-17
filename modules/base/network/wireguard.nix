{ config, lib, pkgs, ... }:

let
  data = {
    fairphone = {
      pubkey = "mHAq+2AP1EZdlSZIxA8UCret8EStrR3nEIU2x6NVETE=";
      ip = "10.240.0.5";
    };
    kharbranth = {
      pubkey = "Zc45PJl+kaa/2GnIs1ObfAmbe640uJ4h1oRn6+qOQHU=";
      privkeyFile = config.age.secrets."files/wireguard/kharbranth.privkey".path;
      ip = "10.240.0.3";
    };
    kholinar = {
      pubkey = "oRA22ymFeNQBeRx6Jyd6Gd8EOUpAv9QSFkGs+Br7yEk=";
      privkeyFile = config.age.secrets."files/wireguard/kholinar.privkey".path;
      ip = "10.240.0.4";
    };
    lasting-integrity = {
      pubkey = "mid3XfCY2jaNK0J6C9ltFLAbxL0IApwMw9K1Z+PU8C0=";
      privkeyFile = config.age.secrets."files/wireguard/lasting-integrity.privkey".path;
      ip = "10.240.0.1";
    };
    urithiru = {
      pubkey = "f4bnm/qNhMW5iXdQcBMmP8IUN6n+pDS15Ikct7QPr0E=";
      privkeyFile = config.age.secrets."files/wireguard/urithiru.privkey".path;
      ip = "10.240.0.2";
    };
  };
  subnet = "10.240.0.0/24";
  pskFile = config.age.secrets."files/wireguard/psk".path;
in
{
  options.chvp.base.network.wireguard = {
    server = lib.mkOption {
      default = false;
      example = true;
    };
    onCorporate = lib.mkOption {
      default = false;
      example = true;
    };
  };
  config = {
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
                PublicKey = data.lasting-integrity.pubkey;
                AllowedIPs = subnet;
                Endpoint =
                  if config.chvp.base.network.wireguard.onCorporate
                  then "127.0.0.1:51820"
                  else "lasting-integrity.vanpetegem.me:51820";
                PresharedKeyFile = pskFile;
                PersistentKeepalive = 25;
              }]);
        };
        networks.wg0 = {
          enable = true;
          name = "wg0";
          address = [ "${data.${config.networking.hostName}.ip}/32" ];
          domains = [ "internal" ];
          dns = [ data.lasting-integrity.ip ];
          linkConfig.MTUBytes = "1342";
          routes = [
            (
              if config.chvp.base.network.wireguard.server then {
                Gateway = "${data.${config.networking.hostName}.ip}";
                Destination = subnet;
              } else {
                Gateway = "${data.lasting-integrity.ip}";
                Destination = subnet;
                GatewayOnLink = true;
              }
            )
          ];
        };
      };
      services = {
        udp2raw-server = lib.mkIf config.chvp.base.network.wireguard.server {
          description = "UDP tunnel over TCP for wireguard";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          script = ''
            ${pkgs.udp2raw}/bin/udp2raw -s -l 0.0.0.0:8080 -r 127.0.0.1:51820 \
              -k "$(cat ${config.age.secrets."files/wireguard/udp2raw".path})"
          '';
        };
        udp2raw-client = lib.mkIf config.chvp.base.network.wireguard.onCorporate {
          description = "UDP tunnel over TCP for wireguard";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          script = ''
            ${pkgs.udp2raw}/bin/udp2raw -c -l 127.0.0.1:51820 -r 54.38.222.69:8080 \
              -k "$(cat ${config.age.secrets."files/wireguard/udp2raw".path})"
          '';
        };
      };
    };
    age.secrets."files/wireguard/psk" = {
      file = ../../../secrets/files/wireguard/psk.age;
      owner = "systemd-network";
    };
    age.secrets."files/wireguard/${config.networking.hostName}.privkey" = {
      file = ../../../secrets/files/wireguard + "/${config.networking.hostName}.privkey.age";
      owner = "systemd-network";
    };
    age.secrets."files/wireguard/udp2raw".file = ../../../secrets/files/wireguard/udp2raw.age;
  };
}
