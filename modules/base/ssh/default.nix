{ config, lib, pkgs, ... }:
let
  ssh = pkgs.symlinkJoin {
    name = "ssh";
    paths = [
      (
        pkgs.writeShellScriptBin "ssh" ''
          export TERM=xterm-256color
          ${pkgs.openssh}/bin/ssh $@
        ''
      )
      pkgs.openssh
    ];
  };
  base = home: user: {
    programs.ssh = {
      enable = true;
      compression = true;
      controlMaster = "auto";
      controlPersist = "10m";
      hashKnownHosts = true;
      userKnownHostsFile = "${config.chvp.cachePrefix}${home}/.ssh/known_hosts";
      serverAliveInterval = 10;
      extraOptionOverrides = {
        Include = config.age.secrets."files/programs/ssh/host_configuration_${user}".path;
        IdentityFile = "${config.chvp.dataPrefix}${home}/.ssh/id_ed25519";
        HostKeyAlgorithms = "ssh-ed25519-cert-v01@openssh.com,rsa-sha2-512-cert-v01@openssh.com,rsa-sha2-256-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,rsa-sha2-512,rsa-sha2-256,ssh-rsa";
      };
    };
    home.packages = lib.mkIf config.chvp.graphical.enable [ ssh pkgs.sshfs ];
  };
in
{
  home-manager.users.root = { ... }: (base "/root" "root");
  home-manager.users.charlotte = { ... }: (base "/home/charlotte" "charlotte");
  age.secrets."files/programs/ssh/host_configuration_charlotte" = {
    file = ../../../secrets/files/programs/ssh/host_configuration.age;
    owner = "charlotte";
  };
  age.secrets."files/programs/ssh/host_configuration_root" = {
    file = ../../../secrets/files/programs/ssh/host_configuration.age;
  };
}
