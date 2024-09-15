{ config, lib, pkgs, ... }:

{
  options.chvp.base.network.mobile = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
    wireless-interface = lib.mkOption {
      type = lib.types.str;
      example = "wlp2s0";
    };
    wired-interfaces = lib.mkOption {
      example = { "enp0s29f0u1u2" = { macAddress = "10:65:30:85:bb:18"; }; };
    };
  };

  config = with config.chvp.base.network.mobile; lib.mkIf enable {
    environment.systemPackages = [ pkgs.wpa_supplicant_gui ];
    users.users.charlotte.extraGroups = [ "network" ];
    users.groups.network = { };
    networking = {
      useDHCP = false;
      wireless = {
        enable = true;
        interfaces = [ wireless-interface ];
        secretsFile = config.age.secrets."passwords/networks.age".path;
        userControlled = {
          enable = true;
          group = "network";
        };
        networks = {
          "Public Universal Friend".pskRaw = "ext:PSK_PUF";
          AndroidAP.pskRaw = "ext:PSK_AndroidAP";
          draadloosnw.pskRaw = "ext:PSK_draadloosnw";
          werknet.pskRaw = "ext:PSK_werknet";
          Secorima.pskRaw = "ext:PSK_Secorima";
          "down".pskRaw = "ext:PSK_down";
          "Zeus WPI" = {
            pskRaw = "ext:PSK_Zeus";
            hidden = true;
          };
          "Zeus Event 5G".pskRaw = "ext:PSK_Zeus";
          "Rode Kruis-Gent (internet)".pskRaw = "ext:PSK_RKG";
          "DasNetwerk".pskRaw = "ext:PSK_DasNetwerk";
        };
      };
    };
    systemd.network = {
      enable = true;
      networks = {
        "${wireless-interface}" = {
          enable = true;
          DHCP = "yes";
          matchConfig = { Name = wireless-interface; };
          dhcpV4Config = { RouteMetric = 20; };
          ipv6AcceptRAConfig = { RouteMetric = 20; };
        };
      } // lib.mapAttrs
        (name: attrs: {
          enable = true;
          DHCP = "yes";
          matchConfig = { Name = name; };
          dhcpV4Config = { RouteMetric = 10; };
          ipv6AcceptRAConfig = { RouteMetric = 10; };
        } // attrs)
        wired-interfaces;
      wait-online.anyInterface = true;
    };

    age.secrets."passwords/networks.age" = {
      file = ../../../../secrets/passwords/networks.age;
    };
  };
}
