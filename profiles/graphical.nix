{ pkgs, ... }:

{
  imports = [
    ../configurations/adb.nix
    ../configurations/calibre.nix
    ../configurations/citrix.nix
    ../configurations/deluge.nix
    ../configurations/dropbox.nix
    ../configurations/dwarf-fortress.nix
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
    ../configurations/virtualbox.nix
    ../configurations/xdg-dirs.nix
    ../configurations/zeroad.nix
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [
      bluej
      google-chrome
      greenfoot
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

  chvp.gnupg.pinentryFlavor = "qt";
}
