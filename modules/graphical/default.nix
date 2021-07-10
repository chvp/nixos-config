{ config, lib, pkgs, ... }:

{
  imports = [
    ./firefox
    ./gnupg
    ./mail
    ./pass
    ./sound
    ./sway
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
        emacs.extraConfig = [
          ''
            ;; Ligatures in GUI mode
            ;; Should probably switch to ligature.el, but it isn't on MELPA (yet).
            (use-package fira-code-mode :config (when window-system (global-fira-code-mode)))

          ''
        ];
        nix.unfreePackages = [ "google-chrome" ];
      };
      graphical = {
        firefox.enable = lib.mkDefault true;
        gnupg = {
          enable = lib.mkDefault true;
          pinentryFlavor = "qt";
        };
        mail.enable = lib.mkDefault true;
        pass.enable = lib.mkDefault true;
        sound.enable = lib.mkDefault true;
        sway.enable = lib.mkDefault true;
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
        ungoogled-chromium
        youtube-dl
      ];
    };
  };
}
