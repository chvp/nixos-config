{ config, lib, ... }:

{
  options.chvp.ugent.citrix.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.ugent.citrix.enable {
    chvp = {
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
