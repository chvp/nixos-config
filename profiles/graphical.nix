{ pkgs, ... }:

{
  imports = [
    ../configurations/adb.nix
    ../configurations/calibre.nix
    ../configurations/citrix.nix
    ../configurations/deluge.nix
    ../configurations/docker.nix
    ../configurations/dropbox.nix
    ../configurations/element-desktop.nix
    ../configurations/firefox.nix
    ../configurations/fonts.nix
    ../configurations/i3.nix
    ../configurations/kernel.nix
    ../configurations/kitty.nix
    ../configurations/mime.nix
    ../configurations/mounts.nix
    ../configurations/mumble.nix
    ../configurations/networkmanager.nix
    ../configurations/sound.nix
    ../configurations/sway.nix
    ../configurations/syncthing.nix
    ../configurations/steam.nix
    ../configurations/teams.nix
    ../configurations/themes.nix
    ../configurations/thunderbird.nix
    ../configurations/xdg-dirs.nix
    ../configurations/zeroad.nix
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [
      google-chrome
      libreoffice
      mpv
      okular
      pavucontrol
      ranger
      slurp
      sshfs
      ungoogled-chromium
      wf-recorder
      wl-clipboard
      zoom-us
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

  custom.gnupg.pinentryFlavor = "qt";
}
