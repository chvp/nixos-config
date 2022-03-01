{ config, lib, ... }:

{
  options.chvp.work.citrix.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.work.citrix.enable {
    chvp.base = {
      nix.unfreePackages = [ "citrix-workspace" ];
      zfs.homeLinks = [
        { path = ".ICAClient"; type = "data"; }
      ];
    };
    home-manager.users.charlotte = { pkgs, ... }: {
      home.packages = with pkgs; [ citrix_workspace ];
    };
  };
}
