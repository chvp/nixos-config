{ config, lib, ... }:

{
  options.chvp.gnupg.pinentryFlavor = lib.mkOption {
    type = lib.types.str;
    default = "curses";
    example = "qt";
    description = ''
      Pinentry flavor for gnupg.
    '';
  };

  config = {
    chvp.zfs.homeLinks = [
      { path = ".gnupg/crls.d"; type = "data"; }
      { path = ".gnupg/private-keys-v1.d"; type = "data"; }
      { path = ".gnupg/pubring.kbx"; type = "data"; }
      { path = ".gnupg/trustdb.gpg"; type = "data"; }
    ];
    programs.gnupg.agent = {
      enable = true;
    };
    home-manager.users.charlotte = { lib, ... }: {
      home.activation.fixPermissionsCommands = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        mkdir -p /home/charlotte/.gnupg
        chmod u=rwX,go= /home/charlotte/.gnupg
      '';
      programs.gpg.enable = true;
      services.gpg-agent = {
        enable = true;
        defaultCacheTtl = 7200;
        maxCacheTtl = 99999;
        pinentryFlavor = config.chvp.gnupg.pinentryFlavor;
      };
    };
  };
}
