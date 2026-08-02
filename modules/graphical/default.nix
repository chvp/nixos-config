{ config, lib, pkgs, ... }:

{
  imports = [
    ./compositor
    ./firefox
    ./gnupg
    ./mail
    ./nextcloud-client
    ./pass
    ./sound
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
        zfs.homeLinks = [
          { path = ".config/qalculate"; type = "cache"; }
          { path = ".cache/accentor"; type = "cache"; }
          { path = ".local/share/accentor"; type = "cache"; }
        ];
      };
      graphical = {
        compositor.enable = lib.mkDefault true;
        firefox.enable = lib.mkDefault true;
        gnupg = {
          enable = lib.mkDefault true;
          pinentryFlavor = "qt";
        };
        mail.enable = lib.mkDefault true;
        nextcloud-client.enable = lib.mkDefault true;
        pass.enable = lib.mkDefault true;
        sound.enable = lib.mkDefault true;
        terminal.enable = lib.mkDefault true;
        theme.enable = lib.mkDefault true;
        xdg.enable = lib.mkDefault true;
      };
    };

    services.udev.packages = [ pkgs.keychron-udev-rules ];

    home-manager.users.charlotte = { ... }: {
      home.packages = with pkgs; [ gimp mpv kdePackages.okular ranger uni wtype google-chrome accentor-desktop ];
      systemd.user.services.accentord = {
        Unit = {
          Description = "Accentor Desktop daemon";
          PartOf = [ "graphical-session.target" ];
          After = [ "graphical-session.target" ];
        };
        Service = {
          ExecStart = "${pkgs.accentor-desktop}/bin/accentord";
          Restart = "always";
        };
        Install.WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
