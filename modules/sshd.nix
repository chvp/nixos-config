{ config, lib, ... }:

{
  options.chvp.sshd.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.sshd.enable {
    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      permitRootLogin = "prohibit-password";
      hostKeys = [
        { bits = 4096; path = "${config.chvp.dataPrefix}/etc/ssh/ssh_host_rsa_key"; type = "rsa"; }
        { path = "${config.chvp.dataPrefix}/etc/ssh/ssh_host_ed25519_key"; type = "ed25519"; }
      ];
      authorizedKeysFiles = [ "/run/secrets/authorized_keys/%u" ];
    };

    age.secrets."authorized_keys/charlotte" = {
      file = ../secrets/authorized_keys/charlotte.age;
      owner = "charlotte";
    };
    age.secrets."authorized_keys/root".file = ../secrets/authorized_keys/root.age;
  };
}
