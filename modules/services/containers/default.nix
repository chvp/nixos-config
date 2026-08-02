{ config, lib, pkgs, ... }:

{
  options.chvp.services.containers = {
    enable = lib.mkOption {
      default = false;
      example = true;
    };
    externalInterface = lib.mkOption {
      example = "eno3";
    };
  };
  config = {
    networking.nat = lib.mkIf config.chvp.services.containers.enable {
      enable = true;
      enableIPv6 = true;
      internalInterfaces = [ "ve-+" ];
      externalInterface = config.chvp.services.containers.externalInterface;
    };
  };
}
