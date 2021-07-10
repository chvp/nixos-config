{ config, lib, ... }:

{
  imports = [
    ./minecraft
    ./mumble
    ./steam
    ./teeworlds
    ./tetris
    ./zeroad
  ];

  options.chvp.games.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.games.enable {
    chvp.games = {
      minecraft.client = lib.mkDefault true;
      mumble.enable = lib.mkDefault true;
      steam.enable = lib.mkDefault true;
      zeroad.client = lib.mkDefault true;
    };
  };
}
