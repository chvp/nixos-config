{ config, lib, ... }:

{
  options.chvp.gnupg = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
    pinentryFlavor = lib.mkOption {
      type = lib.types.str;
      default = "curses";
      example = "qt";
      description = ''
        Pinentry flavor for gnupg.
      '';
    };
  };

  config = lib.mkIf config.chvp.gnupg.enable {
    chvp.zfs.homeLinks = [
      { path = ".gnupg"; type = "data"; }
    ];
    programs.gnupg.agent = {
      enable = true;
      pinentryFlavor = config.chvp.gnupg.pinentryFlavor;
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
