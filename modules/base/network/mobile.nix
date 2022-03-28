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
    networking = {
      wireless = {
        enable = true;
        interfaces = [ wireless-interface ];
        environmentFile = config.age.secrets."passwords/networks.age".path;
        networks = {
          "Public Universal Friend".psk = "@PSK_PUF@";
          AndroidAP.psk = "@PSK_AndroidAP@";
          draadloosnw.psk = "@PSK_draadloosnw@";
          Secorima.psk = "@PSK_Secorima@";
          "Zeus WPI" = {
            psk = "@PSK_Zeus@";
            hidden = true;
          };
          "Zeus Event 5G".psk = "@PSK_Zeus@";
          eduroam = {
            authProtocols = [ "WPA-EAP" ];
            auth = ''
              eap=PEAP
              identity="@EDUROAM_USER@"
              password="@EDUROAM_PASS@"
            '';
            extraConfig = ''
              phase1="peaplabel=0"
              phase2="auth=MSCHAPV2"
              group=CCMP TKIP
              ca_cert="${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              altsubject_match="DNS:ugnps.ugent.be"
            '';
          };
        };
      };
      interfaces = {
        "${wireless-interface}".useDHCP = true;
      } // lib.mapAttrs (name: attrs: { useDHCP = true; } // attrs) wired-interfaces;
    };

    age.secrets."passwords/networks.age" = {
      file = ../../../secrets/passwords/networks.age;
    };
  };
}
