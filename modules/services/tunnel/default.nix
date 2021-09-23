{ config, pkgs, lib, ... }:

{
  options.chvp.services.tunnel.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.services.tunnel.enable {
    networking.firewall.trustedInterfaces = [ "br-mailcow" ];
    systemd.services.tunnel = {
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      script = "${pkgs.openssh}/bin/ssh -i ${config.age.secrets."files/services/tunnel/key".path} -o ServerAliveInterval=60 -o ExitOnForwardFailure=yes -o ControlPath=none -NT -p $SSH_PORT -L 0.0.0.0:9797:$CONN_HOST:$CONN_PORT $USER@$SSH_HOST";
      serviceConfig = {
        EnvironmentFile = config.age.secrets."files/services/tunnel/env".path;
      };
    };

    age.secrets."files/services/tunnel/key".file = ../../../secrets/files/services/tunnel/key.age;
    age.secrets."files/services/tunnel/env".file = ../../../secrets/files/services/tunnel/env.age;
  };
}
