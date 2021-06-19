{ pkgs, ... }:

{
  imports = [
    ../configurations/adb.nix
    ../configurations/calibre.nix
    ../configurations/citrix.nix
    ../configurations/deluge.nix
    ../configurations/dropbox.nix
    ../configurations/firefox.nix
    ../configurations/fonts.nix
    ../configurations/gnupg.nix
    ../configurations/hledger.nix
    ../configurations/i3.nix
    ../configurations/kitty.nix
    ../configurations/mail.nix
    ../configurations/mime.nix
    ../configurations/mounts.nix
    ../configurations/mumble.nix
    ../configurations/networkmanager.nix
    ../configurations/pass.nix
    ../configurations/sound.nix
    ../configurations/sway.nix
    ../configurations/syncthing.nix
    ../configurations/teams.nix
    ../configurations/themes.nix
    ../configurations/xdg-dirs.nix
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [
      google-chrome
      libreoffice-fresh
      moreutils
      mpv
      okular
      pandoc
      pavucontrol
      qjackctl
      ranger
      slurp
      sshfs
      texlive.combined.scheme-small
      ungoogled-chromium
      unzip
      wf-recorder
      wl-clipboard
      youtube-dl
    ];
    programs.zsh.loginExtra = ''
      if [[ -z "$DISPLAY" ]] && [[ $(tty) = "/dev/tty1" ]]; then
        exec sway
      fi

      if [[ -z "$DISPLAY" ]] && [[ $(tty) = "/dev/tty2" ]]; then
        exec startx
      fi
    '';
  };

  services.fwupd.enable = true;

  chvp = {
    gnupg.pinentryFlavor = "qt";
    nix.unfreePackages = [ "google-chrome" ];
  };
}
