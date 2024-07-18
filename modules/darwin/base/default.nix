{ config, ... }:

let
  username = config.chvp.username;
in
{
  imports = [
    ./nix
  ];

  users.users.${username}.home = "/Users/${username}";
}
