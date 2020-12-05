{ config, lib, pkgs, ... }:

{
  options.chvp.eid.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.eid.enable {
    environment.systemPackages = [ pkgs.eid-mw ];
    services.pcscd = {
      enable = true;
      plugins = [ pkgs.ccid ];
    };
  };
}
