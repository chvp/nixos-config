{ pkgs, ... }:

{
  imports = [
    ../configurations/adb.nix
    ../configurations/citrix.nix
    ../configurations/deluge.nix
    ../configurations/docker.nix
    ../configurations/dropbox.nix
    ../configurations/firefox.nix
    ../configurations/fonts.nix
    ../configurations/i3.nix
    ../configurations/joplin.nix
    ../configurations/kernel.nix
    ../configurations/kitty.nix
    ../configurations/mime.nix
    ../configurations/mounts.nix
    ../configurations/mumble.nix
    ../configurations/networkmanager.nix
    ../configurations/sound.nix
    ../configurations/sway.nix
    ../configurations/syncthing.nix
    ../configurations/teams.nix
    ../configurations/themes.nix
    ../configurations/thunderbird.nix
    ../configurations/xdg-dirs.nix
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [
      chromium
      google-chrome
      libreoffice
      mpv
      okular
      pavucontrol
      ranger
      slurp
      sshfs
      wf-recorder
      wl-clipboard
      zeroad
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
