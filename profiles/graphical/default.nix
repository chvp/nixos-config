{ pkgs, ... }:

{
  imports = [
    ./secret.nix
    ../../programs/dropbox/default.nix
    ../../programs/i3/default.nix
    ../../programs/kitty/default.nix
    ../../programs/syncthing/default.nix
    ../../programs/sway/default.nix
  ];

  services.pcscd = {
    enable = true;
    plugins = [ pkgs.ccid ];
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  fonts = {
    enableFontDir = true;
    fontconfig = {
      enable = true;
      defaultFonts = {
        emoji = [ "Noto Color Emoji" ];
        monospace = [ "Fira Code" ];
        sansSerif = [ "Noto Sans" ];
        serif = [ "Noto Serif" ];
      };
    };
    fonts = with pkgs; [
      fira-code
      fira-code-symbols
      font-awesome_4
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts-extra
    ];
  };

  networking = {
    hosts = { "127.0.0.1" = [ "dodona.localhost" "sandbox.localhost" ]; };
    networkmanager = {
      enable = true;
      packages = [ pkgs.networkmanager-vpnc ];
      wifi.macAddress = "random";
    };
  };

  virtualisation.docker.enable = true;

  users.users.charlotte.extraGroups = [
    "adbusers"
    "docker"
    "input"
    "networkmanager"
    "video"
  ];

  environment.systemPackages = with pkgs; [ eid-mw ];

  programs.adb.enable = true;

  home-manager.users.charlotte = { pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
    home = {
      packages = with pkgs; [
        chromium
        citrix_workspace
        deluge
        (import ../../programs/firefox/default.nix { inherit pkgs; })
        google-chrome
        (gnupg.override { guiSupport = true; })
        hledger
        joplin-desktop
        libreoffice
        moreutils
        mpv
        mumble
        networkmanagerapplet
        okular
        pavucontrol
        ranger
        slurp
        sshfs
        teams
        thunderbird
        vanilla-dmz
        wf-recorder
        wl-clipboard
        xdg-user-dirs
        zeroad
      ];
      file = {
        ".icons/default/index.theme".text = ''
          [Icon Theme]
          Name=Default
          Comment=Default Cursor Theme
          Inherits=Vanilla-DMZ
        '';
      };
    };
    programs.zsh.loginExtra = ''
      if [[ -z "$DISPLAY" ]] && [[ $(tty) = "/dev/tty1" ]]; then
        exec sway
      fi

      if [[ -z "$DISPLAY" ]] && [[ $(tty) = "/dev/tty2" ]]; then
        exec startx
      fi
    '';
    dconf.settings = {
      "org/gnome/desktop/interface" = {
        gtk-theme = "Breeze";
        icon-theme = "breeze";
        cursor-theme = "Vanilla-DMZ";
      };
    };
    gtk = {
      enable = true;
      font = {
        package = pkgs.noto-fonts;
        name = "Noto Sans 10";
      };
      gtk2.extraConfig = ''
        gtk-cursor-theme-name = "Vanilla-DMZ"
        gtk-cursor-theme-size = 0
      '';
      gtk3.extraConfig = {
        gtk-cursor-theme-name = "Vanilla-DMZ";
        gtk-cursor-theme-size = 0;
      };
      iconTheme = {
        package = pkgs.breeze-icons;
        name = "breeze";
      };
      theme = {
        package = pkgs.breeze-gtk;
        name = "Breeze";
      };
    };
    qt = {
      enable = true;
      platformTheme = "gtk";
    };
    xdg = {
      enable = true;
      userDirs = {
        enable = true;
        desktop = "\$HOME/desktop";
        documents = "\$HOME/documents";
        download = "\$HOME/downloads";
        music = "\$HOME/music";
        pictures = "\$HOME/pictures";
        publicShare = "\$HOME/desktop";
        templates = "\$HOME/templates";
        videos = "\$HOME/videos";
      };
    };
  };
}
