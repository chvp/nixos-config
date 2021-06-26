{ config, lib, pkgs, ... }:

{
  options.chvp.graphical = lib.mkOption {
    default = false;
    example = true;
  };

  config = lib.mkIf config.chvp.graphical {
    users.users.charlotte.extraGroups = [ "input" "video" ];
    chvp = {
      calibre.enable = lib.mkDefault true;
      deluge-client.enable = lib.mkDefault true;
      docker.enable = lib.mkDefault true;
      eid.enable = lib.mkDefault true;
      firefox.enable = lib.mkDefault true;
      mail-client.enable = lib.mkDefault true;
      gnupg = {
        enable = lib.mkDefault true;
        pinentryFlavor = lib.mkDefault "qt";
      };
      hledger.enable = lib.mkDefault true;
      networkmanager.enable = lib.mkDefault true;
      nix.unfreePackages = [ "google-chrome" ];
      pass.enable = lib.mkDefault true;
      sound.enable = lib.mkDefault true;
      syncthing-client.enable = lib.mkDefault true;
      sway.enable = lib.mkDefault true;
      terminal.enable = lib.mkDefault true;
      theming.enable = lib.mkDefault true;
      ugent.enable = lib.mkDefault true;
      xdg.enable = lib.mkDefault true;
      zotero.enable = lib.mkDefault true;
    };

    home-manager.users.charlotte = { ... }: {
      home.packages = with pkgs; [
        google-chrome
        libreoffice-fresh
        mpv
        okular
        pandoc
        ranger
        texlive.combined.scheme-small
        ungoogled-chromium
        youtube-dl
      ];
    };
  };
}
