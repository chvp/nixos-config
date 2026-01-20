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
    users.users.charlotte.extraGroups = [ "networkmanager" ];
    networking = {
      networkmanager = {
        enable = true;
        ensureProfiles = {
          environmentFiles = [ config.age.secrets."passwords/networks.age".path ];
          profiles = {
            "MeetDistrict Member" = {
              "802-1x" = {
                ca-cert = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
                domain-suffix-match = "services.meetdistrict.com";
                eap = "peap;";
                identity = "$USER_MEETDISTRICT";
                password = "$PASS_MEETDISTRICT";
                phase2-auth = "mschapv2";
              };
              connection = {
                id = "MeetDistrict Member";
                interface-name = config.chvp.base.network.mobile.wireless-interface;
                type = "wifi";
                uuid = "f8f1ee59-caf5-4c73-b0ef-bb4c3a667ce9";
              };
              ipv4 = {
                method = "auto";
              };
              ipv6 = {
                addr-gen-mode = "default";
                method = "auto";
              };
              proxy = { };
              wifi = {
                mode = "infrastructure";
                ssid = "MeetDistrict Member";
              };
              wifi-security = {
                auth-alg = "open";
                key-mgmt = "wpa-eap";
              };
            };
          } // (lib.mapAttrs
            (name: value: {
              connection = {
                id = name;
                interface-name = config.chvp.base.network.mobile.wireless-interface;
                type = "wifi";
                uuid = value.uuid;
              };
              ipv4 = {
                method = "auto";
              };
              ipv6 = {
                addr-gen-mode = "default";
                method = "auto";
              };
              proxy = { };
              wifi = {
                mode = "infrastructure";
                ssid = name;
              };
              wifi-security = {
                auth-alg = "open";
                key-mgmt = "wpa-psk";
                psk = value.psk;
              };
            })
            {
              AndroidAP = { psk = "$PSK_ANDROIDAP"; uuid = "8db42d01-58c4-4782-86a7-098a773b9088"; };
              "DasNetwerk" = { psk = "$PSK_DASNETWERK"; uuid = "2a38fd24-858a-40cd-9b2f-7a4711e1ec4c"; };
              "down" = { psk = "$PSK_DOWN"; uuid = "8c6bb510-c53d-4df1-98b6-40de6ea2350a"; };
              draadloosnw = { psk = "$PSK_DRAADLOOSNW"; uuid = "6e1bb0aa-db09-44a4-bb69-30ddce141e1e"; };
              "Public Universal Friend" = { psk = "$PSK_PUF"; uuid = "5aae994c-5f09-46f6-b84c-8e260faedf4b"; };
              Secorima = { psk = "$PSK_SECORIMA"; uuid = "180c4990-5cf2-44d0-bf66-0b879db25e0c"; };
              "Rode Kruis-Gent (internet)" = { psk = "$PSK_RKG"; uuid = "3e5609d5-ffdc-4c08-9522-02b8c9f92435"; };
              werknet = { psk = "$PSK_WERKNET"; uuid = "363a064d-b829-4f18-9976-a0910ec1ba60"; };
              "Zeus Event 5G" = { psk = "$PSK_ZEUS"; uuid = "a9d45b71-4482-4192-8108-83911d6843a7"; };
              "Zeus WPI" = { psk = "$PSK_ZEUS"; uuid = "78a8ab49-1228-4d4c-bfe6-d8c8d482b78f"; };
            });
        };
      };
    };

    home-manager.users.charlotte = { ... }: {
      services.network-manager-applet.enable = true;
    };

    age.secrets."passwords/networks.age".file = ../../../../secrets/passwords/networks.age;
  };
}
