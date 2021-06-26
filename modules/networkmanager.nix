{ config, lib, pkgs, ... }:

{
  options.chvp.networkmanager.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.networkmanager.enable {
    chvp.zfs.systemLinks = [
      { path = "/etc/NetworkManager/system-connections"; type = "data"; }
    ];
    networking.networkmanager = {
      enable = true;
      wifi.macAddress = "random";
    };

    users.users.charlotte.extraGroups = [ "networkmanager" ];
    home-manager.users.charlotte = { ... }: {
      home.packages = with pkgs; [ networkmanagerapplet ];
    };
  };
}
