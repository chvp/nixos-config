{ config, lib, pkgs, ... }:

{
  options.chvp.work.mounts.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.work.mounts.enable {
    fileSystems =
      let
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
      in
      {
        "/mnt/ugent/files" = {
          device = "//files.ugent.be/ecvpeteg";
          fsType = "cifs";
          options = [ "credentials=${config.age.secrets."passwords/ugent-mount-credentials".path},${automount_opts},users,vers=3.11,noperm,domain=UGENT,sec=ntlmv2i" ];
          noCheck = true;
        };
        "/mnt/ugent/webhost" = {
          device = "//webhost.ugent.be/ecvpeteg";
          fsType = "cifs";
          options = [ "credentials=${config.age.secrets."passwords/ugent-mount-credentials".path},${automount_opts},users,vers=3.0" ];
          noCheck = true;
        };
      };

    age.secrets."passwords/ugent-mount-credentials".file = ../../../secrets/passwords/ugent-mount-credentials.age;

    environment.systemPackages = [ pkgs.keyutils ];
    # Remove this once https://github.com/NixOS/nixpkgs/issues/34638 is resolved
    # request-key expects a configuration file under /etc
    environment.etc."request-key.conf" = {
      text =
        let
          upcall = "${pkgs.cifs-utils}/bin/cifs.upcall";
          keyctl = "${pkgs.keyutils}/bin/keyctl";
        in
        ''
          #OP     TYPE          DESCRIPTION  CALLOUT_INFO  PROGRAM
          # -t is required for DFS share servers...
          create  cifs.spnego   *            *             ${upcall} -t %k
          create  dns_resolver  *            *             ${upcall} %k
          # Everything below this point is essentially the default configuration,
          # modified minimally to work under NixOS. Notably, it provides debug
          # logging.
          create  user          debug:*      negate        ${keyctl} negate %k 30 %S
          create  user          debug:*      rejected      ${keyctl} reject %k 30 %c %S
          create  user          debug:*      expired       ${keyctl} reject %k 30 %c %S
          create  user          debug:*      revoked       ${keyctl} reject %k 30 %c %S
          create  user          debug:loop:* *             |${pkgs.coreutils}/bin/cat
          create  user          debug:*      *             ${pkgs.keyutils}/share/keyutils/request-key-debug.sh %k %d %c %S
          negate  *             *            *             ${keyctl} negate %k 30 %S
        '';
    };
  };
}
