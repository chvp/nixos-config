{ config, ... }:

let
  username = config.chvp.username;
in
{
  imports = [
    ./emacs
    ./nix
    ./wireguard
  ];

  system.primaryUser = username;
  users.users.${username}.home = "/Users/${username}";
}
