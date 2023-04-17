{ config, lib, pkgs, ... }:

{
  imports = [
    ./compositor
    ./firefox
    ./gnupg
    ./mail
    ./pass
    ./sound
    ./syncthing
    ./terminal
    ./theme
    ./xdg
  ];

  options.chvp.graphical.enable = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical.enable {
    users.users.charlotte.extraGroups = [ "input" "video" ];
    chvp = {
      base = {
        nix.unfreePackages = [ "google-chrome" ];
      };
      graphical = {
        compositor.enable = lib.mkDefault true;
        firefox.enable = lib.mkDefault true;
        gnupg = {
          enable = lib.mkDefault true;
          pinentryFlavor = "qt";
        };
        mail.enable = lib.mkDefault true;
        pass.enable = lib.mkDefault true;
        sound.enable = lib.mkDefault true;
        syncthing.enable = lib.mkDefault true;
        terminal.enable = lib.mkDefault true;
        theme.enable = lib.mkDefault true;
        xdg.enable = lib.mkDefault true;
      };
    };

    home-manager.users.charlotte = { ... }: {
      home.packages = with pkgs; [
        google-chrome
        mpv
        okular
        ranger
        uni
      ];
    };
  };
}
