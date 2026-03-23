{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  options.chvp.programs.htop.enable = lib.mkOption {
    default = true;
    example = false;
  };

  config = lib.mkIf config.chvp.programs.htop.enable {
    home-manager.users.${username} = { config, ... }: {
      home.packages = [ pkgs.htop ];
      programs.htop = {
        enable = true;
        settings = {
          hide_userland_threads = 1;
          fields = with config.lib.htop.fields; [
            PID USER PRIORITY NICE M_VIRT M_RESIDENT M_SWAP M_SHARE STATE PERCENT_CPU PERCENT_MEM TIME COMM
          ];
        };
      };
    };
  };
}
