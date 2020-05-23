{ pkgs, ... }:

{
  custom.zfs.systemLinks = [
    { path = "/etc/NetworkManager/system-connections"; type = "data"; }
  ];

  networking = {
    hosts = { "127.0.0.1" = [ "dodona.localhost" "sandbox.localhost" ]; };
    networkmanager = {
      enable = true;
      packages = [ pkgs.networkmanager-vpnc ];
      wifi.macAddress = "random";
    };
  };

  users.users.charlotte.extraGroups = [
    "networkmanager"
  ];
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [
      networkmanagerapplet
    ];
  };
}
