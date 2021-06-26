{ config, lib, ... }:

{
  imports = [
    ./vpn.nix
    ./citrix.nix
    ./mounts.nix
    ./teams.nix
  ];

  options.chvp.ugent.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.ugent.enable {
    chvp.ugent = {
      citrix.enable = lib.mkDefault true;
      vpn.enable = lib.mkDefault true;
      mounts.enable = lib.mkDefault true;
      teams.enable = lib.mkDefault true;
    };
  };
}
