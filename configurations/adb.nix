{ ... }:

{
  programs.adb.enable = true;
  users.users.charlotte.extraGroups = [ "adbusers" ];
}
