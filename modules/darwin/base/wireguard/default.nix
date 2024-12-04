{ config, ... }:

let
  data = config.chvp.base.network.wireguard.data;
  subnet = config.chvp.base.network.wireguard.subnet;
  pskFile = config.chvp.base.network.wireguard.pskFile;
in
{
  # networking.wg-quick.interfaces."wg0" = {
  #   address = [ "${data.${config.networking.hostName}.ip}/32" ];
  #   autostart = true;
  #   dns = [ data.marabethia.ip ];
  #   mtu = 1342;
  #   peers = [
  #     {
  #       allowedIPs = [ subnet ];
  #       endpoint = "marabethia.vanpetegem.me:51820";
  #       presharedKeyFile = pskFile;
  #       persistentKeepalive = 25;
  #       publicKey = data.marabethia.pubkey;
  #     }
  #   ];
  #   privateKeyFile = data.${config.networking.hostName}.privkeyFile;
  # };
}
