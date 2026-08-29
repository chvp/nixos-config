{ config, lib, ... }:

let
  username = config.chvp.username;
in
{
  options.chvp.base.ssh.allowRootLogin = lib.mkOption {
    default = true;
    example = false;
  };

  config = {
    chvp.base.zfs = {
      ensureSystemExists = [ "${config.chvp.dataPrefix}/etc/ssh" ];
      ensureHomeExists = [ ".ssh" ];
    };
    services.openssh = {
      enable = true;
      hostKeys = [
        {
          bits = 4096;
          path = "${config.chvp.dataPrefix}/etc/ssh/ssh_host_rsa_key";
          type = "rsa";
        }
        {
          path = "${config.chvp.dataPrefix}/etc/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = if config.chvp.base.ssh.allowRootLogin then "prohibit-password" else "no";
      };
    };

    age.secrets."authorized_keys/root" = {
      file = ../../../secrets/authorized_keys/root.age;
      path = "/root/.ssh/authorized_keys";
      symlink = false;
    };
    age.secrets."authorized_keys/charlotte" = {
      file = ../../../secrets/authorized_keys/charlotte.age;
      owner = username;
      path = "/home/${username}/.ssh/authorized_keys";
      symlink = false;
    };
  };
}
