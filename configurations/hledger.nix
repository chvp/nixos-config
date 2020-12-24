{ pkgs, ... }:
{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ pkgs.hledger ];
  };
}
