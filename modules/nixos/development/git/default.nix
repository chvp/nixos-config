{ config, lib, pkgs, ... }:

{
  home-manager.users.charlotte.programs.git = lib.mkIf config.chvp.development.git.enable {
  };
}
