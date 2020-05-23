{ pkgs, ... }:

{
  imports = [ ./mounts/secret.nix ];

  environment.systemPackages = [ pkgs.keyutils ];
  # Remove this once https://github.com/NixOS/nixpkgs/issues/34638 is resolved
  # The TL;DR is: the kernel calls out to the hard-coded path of
  # /sbin/request-key as part of its CIFS auth process, which of course does
  # not exist on NixOS due to the usage of Nix store paths.
  system.activationScripts.symlink-requestkey = ''
    if [ ! -d /sbin ]; then
      mkdir /sbin
    fi
    ln -sfn /run/current-system/sw/bin/request-key /sbin/request-key
  '';
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
}
