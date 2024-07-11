{ config, lib, ... }:

{
  imports = [
    ./minecraft
    ./mumble
    ./particles
    ./steam
    ./tetris
  ];

  options.chvp.games.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.games.enable {
    chvp.games = {
      minecraft.client = lib.mkDefault false;
      mumble.enable = lib.mkDefault true;
      steam.enable = lib.mkDefault true;
    };
  };
}
