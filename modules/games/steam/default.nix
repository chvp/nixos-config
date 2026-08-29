{ config, lib, pkgs, ... }:

let
  username = config.chvp.username;
in
{
  options.chvp.games.steam.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.games.steam.enable {
    hardware.graphics = {
      enable32Bit = true;
      extraPackages = with pkgs.pkgsi686Linux; [ libva ];
    };
    services.pipewire.alsa.support32Bit = true;
    chvp.base = {
      zfs.homeLinks = [
        { path = ".paradoxlauncher"; type = "cache"; }
        { path = ".steam"; type = "cache"; }
        { path = ".local/share/Steam"; type = "cache"; }
        { path = ".local/share/Paradox Interactive"; type = "cache"; }
      ];
    };

    home-manager.users.${username} = {
      home.packages = [ pkgs.steam pkgs.protontricks pkgs.wine ];
    };
  };
}
