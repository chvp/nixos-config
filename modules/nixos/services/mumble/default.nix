{ config, pkgs, lib, ... }:

{
  options.chvp.services.mumble.enable = lib.mkOption {
    default = false;
    example = true;
  };


  config = lib.mkIf config.chvp.services.mumble.enable {
    services.murmur = {
      enable = true;
      openFirewall = true;
      password = "$MURMURD_PASSWORD";
    };
    age.secrets."passwords/services/murmur" = {
      file = ../../../../secrets/passwords/services/murmur.age;
      owner = "murmur";
    };
  };
}
