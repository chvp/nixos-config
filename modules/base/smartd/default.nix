{ config, lib, pkgs, ... }:

{
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
}
