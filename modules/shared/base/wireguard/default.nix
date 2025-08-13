{ config, lib, ... }:

let
  data = {
    elendel = {
      pubkey = "XqDE9FuVWnQOANsyDkbZMPzJ10MoAKkwewmP/kujlGY=";
      privkeyFile = config.age.secrets."files/wireguard/elendel.privkey".path;
      ip = "10.240.0.7";
    };
    fairphone = {
      pubkey = "mHAq+2AP1EZdlSZIxA8UCret8EStrR3nEIU2x6NVETE=";
      ip = "10.240.0.4";
    };
    kharbranth = {
      pubkey = "qfDLgVt4veWIpAjKWkI9di18hkLF+UxpAs2k89ZkQ0A=";
      privkeyFile = config.age.secrets."files/wireguard/kharbranth.privkey".path;
      ip = "10.240.0.2";
    };
    kholinar = {
      pubkey = "oRA22ymFeNQBeRx6Jyd6Gd8EOUpAv9QSFkGs+Br7yEk=";
      privkeyFile = config.age.secrets."files/wireguard/kholinar.privkey".path;
      ip = "10.240.0.3";
    };
    marabethia = {
      pubkey = "h451oXBTzim1POLmnJC1OtFzbIXyxg6d5qpFFdHLbRs=";
      privkeyFile = config.age.secrets."files/wireguard/marabethia.privkey".path;
      ip = "10.240.0.1";
    };
    thaylen-city = {
      pubkey = "O0q2/W7dRM4LvAL9MSDZqAbGSzqi8AHLVl1sJsRDsUY=";
      privkeyFile = config.age.secrets."files/wireguard/thaylen-city.privkey".path;
      ip = "10.240.0.5";
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
    data = lib.mkOption {
      default = data;
      readOnly = true;
    };
    subnet = lib.mkOption {
      default = subnet;
      readOnly = true;
    };
    pskFile = lib.mkOption {
      default = pskFile;
      readOnly = true;
    };
  };
  config = {
    age.secrets."files/wireguard/psk".file = ../../../../secrets/files/wireguard/psk.age;
    age.secrets."files/wireguard/${config.networking.hostName}.privkey".file = ../../../../secrets/files/wireguard + "/${config.networking.hostName}.privkey.age";
  };
}
