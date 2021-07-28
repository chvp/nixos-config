{ config, lib, pkgs, ... }:

{
  options.chvp.programs.eid.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.programs.eid.enable {
    environment.systemPackages = [ pkgs.eid-mw ];
    services.pcscd = {
      enable = true;
      plugins = [ pkgs.ccid ];
    };
  };
}
