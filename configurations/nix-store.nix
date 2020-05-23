{ ... }:

{
  nix = {
    gc = {
      automatic = true;
      dates = "hourly";
      options = "--delete-older-than 7d";
    };
    optimise = {
      automatic = true;
      dates = [ "hourly" ];
    };
  };

  system.autoUpgrade = {
    allowReboot = false;
    enable = true;
    dates = "hourly";
  };

  home-manager.users.charlotte = { ... }: {
    nixpkgs.config.allowUnfree = true;
  };
}
