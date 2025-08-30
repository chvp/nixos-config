{ config, lib, pkgs, ... }:
let
  ssh = pkgs.symlinkJoin {
    name = "ssh";
    paths = [
      (
        pkgs.writeShellScriptBin "ssh" ''
          export TERM=xterm-256color
          ${pkgs.openssh}/bin/ssh "$@"
        ''
      )
      pkgs.openssh
    ];
  };
  base = home: user: {
    programs.ssh = {
      enable = true;
      package = if config.chvp.graphical.enable then ssh else pkgs.openssh;
      enableDefaultConfig = false;
      matchBlocks."*" = {
        addKeysToAgent = "no";
        compression = true;
        controlMaster = "auto";
        controlPath = "~/.ssh/master-%r@%n:%p";
        controlPersist = "10m";
        forwardAgent = false;
        hashKnownHosts = true;
        identityFile = "${config.chvp.dataPrefix}${home}/.ssh/id_ed25519";
        serverAliveInterval = 10;
        serverAliveCountMax = 3;
        userKnownHostsFile = "${config.chvp.cachePrefix}${home}/.ssh/known_hosts";
      };
      includes = [
        config.age.secrets."files/programs/ssh/host_configuration_${user}".path
      ];
    };
    home.packages = lib.mkIf config.chvp.graphical.enable [ pkgs.sshfs ];
  };
in
{
  home-manager.users.root = { ... }: (base "/root" "root");
  home-manager.users.charlotte = { ... }: (base "/home/charlotte" "charlotte");
  age.secrets."files/programs/ssh/host_configuration_charlotte" = {
    file = ../../../../secrets/files/programs/ssh/host_configuration.age;
    owner = "charlotte";
  };
  age.secrets."files/programs/ssh/host_configuration_root" = {
    file = ../../../../secrets/files/programs/ssh/host_configuration.age;
  };
}
