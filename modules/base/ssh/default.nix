{
  config,
  lib,
  pkgs,
  ...
}:
let
  username = config.chvp.username;
  ssh = pkgs.symlinkJoin {
    name = "ssh";
    paths = [
      (pkgs.writeShellScriptBin "ssh" ''
        export TERM=xterm-256color
        ${pkgs.openssh}/bin/ssh "$@"
      '')
      pkgs.openssh
    ];
  };
  base = home: user: {
    programs.ssh = {
      enable = true;
      package = if config.chvp.graphical.enable then ssh else pkgs.openssh;
      enableDefaultConfig = false;
      settings = {
        "Host *" = {
          AddKeysToAgent = "no";
          Compression = true;
          ControlMaster = "auto";
          ControlPath = "~/.ssh/master-%r@%n:%p";
          ControlPersist = "10m";
          ForwardAgent = false;
          HashKnownHosts = true;
          IdentityFile = "${config.chvp.dataPrefix}${home}/.ssh/id_ed25519";
          ServerAliveInterval = 10;
          ServerAliveCountMax = 3;
          UserKnownHostsFile = "${config.chvp.cachePrefix}${home}/.ssh/known_hosts";
        };
      };
      includes = [
        config.age.secrets."files/programs/ssh/host_configuration_${user}".path
      ];
    };
    home.packages = lib.mkIf config.chvp.graphical.enable [ pkgs.sshfs ];
  };
in
{
  home-manager.users.root = base "/root" "root";
  home-manager.users.${username} = base "/home/${username}" username;
  programs.fuse.enable = config.chvp.graphical.enable;
  age.secrets."files/programs/ssh/host_configuration_${username}" = {
    file = ../../../secrets/files/programs/ssh/host_configuration.age;
    owner = username;
  };
  age.secrets."files/programs/ssh/host_configuration_root" = {
    file = ../../../secrets/files/programs/ssh/host_configuration.age;
  };
}
