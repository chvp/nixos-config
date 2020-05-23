{ ... }:

{
  custom.zfs.homeLinks = [
    { path = ".cache/nix-index"; type = "cache"; }
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [
      nix-index
    ];
    systemd.user = {
      services.nix-index = {
        Unit = {
          Description = "Service to run nix-index";
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.nix-index}/bin/nix-index";
        };
      };
      timers.nix-index = {
        Unit = {
          Description = "Timer that starts nix-index every two hours";
          PartOf = [ "nix-index.service" ];
        };
        Timer = {
          OnCalendar = "00/2:30";
        };
        Install = {
          WantedBy = [ "default.target" ];
        };
      };
    };
  };
}
