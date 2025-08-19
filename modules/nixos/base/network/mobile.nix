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
      default = { };
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
          "Public Universal Friend".pskRaw = "ext:psk_puf";
          AndroidAP.pskRaw = "ext:psk_androidap";
          draadloosnw.pskRaw = "ext:psk_draadloosnw";
          werknet.pskRaw = "ext:psk_werknet";
          Secorima.pskRaw = "ext:psk_secorima";
          "down".pskRaw = "ext:psk_down";
          "Zeus WPI" = {
            pskRaw = "ext:psk_zeus";
            hidden = true;
          };
          "Zeus Event 5G".pskRaw = "ext:psk_zeus";
          "Rode Kruis-Gent (internet)".pskRaw = "ext:psk_rkg";
          "DasNetwerk".pskRaw = "ext:psk_dasnetwerk";
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
          linkConfig = attrs;
          dhcpV4Config = { RouteMetric = 10; };
          ipv6AcceptRAConfig = { RouteMetric = 10; };
        })
        wired-interfaces;
      wait-online.anyInterface = true;
    };

    age.secrets."passwords/networks.age" = {
      file = ../../../../secrets/passwords/networks.age;
      group = "network";
    };
  };
}
