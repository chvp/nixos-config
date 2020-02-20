{ pkgs, ... }:

{
  imports = [
    ./secret.nix
    ../../programs/dropbox/default.nix
    ../../programs/kitty/default.nix
    ../../programs/neovim/default.nix
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
      font-awesome
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts-extra
    ];
  };

  users.users.charlotte.extraGroups = [ "networkmanager" "video" "input" ];

  environment.systemPackages = with pkgs; [ eid-mw ];

  system.autoUpgrade.enable = true;

  home-manager.users.charlotte = { pkgs, ... }: {
    nixpkgs = {
      config = {
        allowUnfree = true;
      };
    };
    home = {
      packages = with pkgs; [
        chromium
        firefox
        hledger
        libreoffice
        moreutils
        mpv
        okular
        pavucontrol
        rambox
        ranger
        slurp
        sshfs
        thunderbird
        vanilla-dmz
        wf-recorder
        wl-clipboard
        xdg-user-dirs
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
    '';
    dconf.settings = {
      "org/gnome/desktop/interface" = {
        gtk-theme = "Arc";
        icon-theme = "Arc";
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
        package = pkgs.arc-icon-theme;
        name = "Arc";
      };
      theme = {
        package = pkgs.arc-theme;
        name = "Arc";
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
