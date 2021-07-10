{ config, lib, pkgs, ... }:

{
  options.chvp.games.steam.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.games.steam.enable {
    hardware.opengl = {
      driSupport32Bit = true;
      extraPackages = with pkgs.pkgsi686Linux; [ libva ];
    };
    services.pipewire.alsa.support32Bit = true;
    chvp.base = {
      nix.unfreePackages = [ "steam" "steam-original" "steam-runtime" ];
      zfs.homeLinks = [
        { path = ".paradoxlauncher"; type = "cache"; }
        { path = ".steam"; type = "cache"; }
        { path = ".local/share/Steam"; type = "cache"; }
        { path = ".local/share/Paradox Interactive"; type = "cache"; }
      ];
    };

    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = [ pkgs.steam ];
    };
  };
}
