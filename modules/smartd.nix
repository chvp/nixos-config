{ config, lib, pkgs, ... }:

{
  options.chvp.smartd.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.smartd.enable {
    chvp.globalMailer.enable = true;
    environment.systemPackages = [ pkgs.smartmontools ];
    services.smartd = {
      enable = true;
      autodetect = true;
      notifications = {
        mail = {
          enable = true;
          sender = "${config.networking.hostName}@vanpetegem.me";
          recipient = "webmaster@vanpetegem.me";
        };
        wall.enable = false;
      };
    };
  };
}
