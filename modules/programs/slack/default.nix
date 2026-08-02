{ config, lib, pkgs, ... }:

{
  options.chvp.programs.slack.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.programs.slack.enable {
    chvp.base = {
      zfs.homeLinks = [
        { path = ".config/Slack"; type = "cache"; }
      ];
      nix.unfreePackages = [ "slack" ];
    };

    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = with pkgs; [ slack ];
    };
  };
}
