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

  users.users.${username}.home = "/Users/${username}";
}
