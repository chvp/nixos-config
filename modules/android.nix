{ config, lib, ... }:

{
  options.chvp.android.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.android.enable {
    chvp.zfs.homeLinks = [{ path = ".android"; type = "cache"; }];
    programs.adb.enable = true;
    users.users.charlotte.extraGroups = [ "adbusers" ];
  };
}
