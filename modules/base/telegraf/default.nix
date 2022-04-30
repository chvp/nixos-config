{ config, lib, pkgs, ... }:

{
  services.telegraf = {
    enable = true;
    extraConfig = {
      agent = {
        interval = "10s";
        round_interval = true;
        metric_batch_size = 1000;
        metric_buffer_limit = 10000;
        collection_jitter = "0s";
        flush_interval = "10s";
        flush_jitter = "0s";
        precision = "0s";
        omit_hostname = false;
      };
      outputs.influxdb_v2 = {
        urls = [ "https://stats.chvp.be:8086" ];
        token = "$TOKEN";
        organization = "default";
        bucket = "default";
      };
      inputs = {
        cpu = {
          percpu = true;
          totalcpu = true;
          collect_cpu_time = false;
          report_active = false;
        };
        diskio = { };
        exec = {
          commands = [ "${pkgs.zfs}/libexec/zfs/zpool_influxdb" ];
          timeout = "5s";
          data_format = "influx";
        };
        kernel = { };
        mem = { };
        processes = { };
        swap = { };
        system = { };
      };
    };
    environmentFiles = [ config.age.secrets."passwords/services/telegraf-env".path ];
  };
  age.secrets."passwords/services/telegraf-env" = {
    file = ../../../secrets/passwords/services/telegraf-env.age;
    owner = "telegraf";
  };
}
