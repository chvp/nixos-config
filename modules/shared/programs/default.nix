{ config, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  imports = [
    ./hledger
    ./htop
  ];

  chvp.base.zfs.homeLinks = [
    { path = ".local/share/accentor"; type = "cache"; }
  ];

  home-manager.users.${username} = {
    home.packages = with pkgs; [ jq xan yt-dlp libqalculate accentor-desktop ];
    systemd.user.services.accentord = {
      Unit = {
        Description = "Accentor Desktop daemon";
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "${pkgs.accentor-desktop}/bin/accentord";
        Restart = "always";
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
