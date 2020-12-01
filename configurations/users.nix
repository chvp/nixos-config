{ pkgs, ... }:

{
  users.users.charlotte.extraGroups = [ "input" "video" ];
}
