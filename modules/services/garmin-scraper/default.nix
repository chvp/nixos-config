{ config, lib, pkgs, ... }:

let
  garmin2influx = pkgs.writers.writePython3Bin "garmin2influx" {
    libraries = with pkgs.python3Packages; [ garminconnect influxdb-client ];
  } (builtins.readFile ./garmin2influx.py);
in
{
  options.chvp.services.garmin-scraper.enable = lib.mkEnableOption "garmin scraper";

  config = lib.mkIf config.chvp.services.garmin-scraper.enable {
    # Install in environment to allow manual data collection
    environment.systemPackages = [ garmin2influx ];
    systemd = {
      services.garmin2influx = {
        description = "Garmin health data importer";
        restartIfChanged = false;
        unitConfig.X-StopOnRemoval = false;
        serviceConfig = {
          EnvironmentFile = config.age.secrets."passwords/services/garmin2influx-env".path;
          Type = "oneshot";
          User = "charlotte";
          Group = "users";
          ExecStart = "${garmin2influx}/bin/garmin2influx";
          RestartSec = "5s";
          Restart = "on-failure";
        };
        startAt = "02/4:00";
      };
      timers.garmin2influx.timerConfig.RandomizedDelaySec = "30min";
    };
    age.secrets."passwords/services/garmin2influx-env" = {
      file = ../../../secrets/passwords/services/garmin2influx-env.age;
      owner = "charlotte";
    };
  };
}
