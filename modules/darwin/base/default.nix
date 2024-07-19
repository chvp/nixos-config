{ config, ... }:

let
  username = config.chvp.username;
in
{
  imports = [
    ./emacs
    ./nix
  ];

  users.users.${username}.home = "/Users/${username}";
}
