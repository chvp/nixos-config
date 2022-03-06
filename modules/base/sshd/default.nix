{ config, lib, ... }:

{
  chvp.base.zfs = {
    ensureSystemExists = [ "${config.chvp.dataPrefix}/etc/ssh" ];
    ensureHomeExists = [ ".ssh" ];
  };
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    permitRootLogin = "prohibit-password";
    hostKeys = [
      { bits = 4096; path = "${config.chvp.dataPrefix}/etc/ssh/ssh_host_rsa_key"; type = "rsa"; }
      { path = "${config.chvp.dataPrefix}/etc/ssh/ssh_host_ed25519_key"; type = "ed25519"; }
    ];
  };

  age.secrets."authorized_keys/root" = {
    file = ../../../secrets/authorized_keys/root.age;
    path = "/root/.ssh/authorized_keys";
    symlink = false;
  };
  age.secrets."authorized_keys/charlotte" = {
    file = ../../../secrets/authorized_keys/charlotte.age;
    owner = "charlotte";
    path = "/home/charlotte/.ssh/authorized_keys";
    symlink = false;
  };
}
