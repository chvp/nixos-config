{ lib, pkgs, nixosConfigurations, ... }:

{
  imports = [
    ./hardware.nix
    ./secret.nix
  ];

  time.timeZone = "Europe/Berlin";

  networking.hostId = "079e60ba";

  environment.etc = lib.mapAttrs' (n: v: { name = "pinned-hosts/${n}"; value = { source = v.config.system.build.toplevel.outPath; }; })
    (lib.filterAttrs (n: _: n != "urithiru") nixosConfigurations);

  # Machine-specific module settings
  chvp = {
    stateVersion = "20.09";
    base = {
      nix.enableDirenv = false;
      network.ovh.enable = true;
      zfs = {
        backups = [
          {
            path = "zroot/safe/data";
            remotePath = "zdata/recv/urithiru/safe/data";
            fast = true;
            location = "192.168.0.2";
          }
          {
            path = "zdata/data";
            remotePath = "zdata/data";
            fast = false;
            location = "192.168.0.2";
          }
        ];
        rootDataset = "zroot/local/root";
      };
    };
    games = {
      teeworlds.server = false;
      zeroad.server = true;
    };
    services = {
      accentor.enable = true;
      data-access.enable = true;
    };
  };
}
