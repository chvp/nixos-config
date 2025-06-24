{ config, pkgs, ... }:

{
  chvp = {
    homeStateVersion = "24.11";
    systemStateVersion = 4;
    username = "charlotte.vanpetegem";
    development = {
      enable = true;
      git.email = "charlotte.vanpetegem@silverfin.com";
    };
    programs.hledger.enable = true;
  };
  networking.computerName = "Thaylen City";

  age.identityPaths =
    [
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_rsa_key"
    ];

  fonts.packages = with pkgs; [
    hack-font
    font-awesome
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-emoji
    noto-fonts-extra
    roboto
  ];

  homebrew = {
    enable = true;
    brews = [
      "docker-compose"
    ];
    casks = [
      "background-music"
      "calibre"
      "cyberduck"
      "docker-desktop"
      "inkscape"
      "jordanbaird-ice"
      "keepassxc"
      "libreoffice"
      "musicbrainz-picard"
      "platypus"
      "rubymine"
      "sf-symbols"
      "stolendata-mpv"
      "swiftbar"
      "tsh"
    ];
    global.brewfile = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };
  };
  security.pam.services.sudo_local.touchIdAuth = true;
  services.openssh.enable = true;
  system = {
    defaults = {
      dock = {
        appswitcher-all-displays = true;
        autohide = true;
        launchanim = false;
        minimize-to-application = true;
        show-recents = false;
      };
      finder = {
        AppleShowAllExtensions = true;
        FXEnableExtensionChangeWarning = false;
        FXPreferredViewStyle = "Nlsv";
        ShowPathbar = true;
      };
      loginwindow.GuestEnabled = false;
      menuExtraClock.ShowSeconds = true;
      screencapture.location = "/Users/charlotte.vanpetegem/Pictures/Nextcloud/Inbox/Screenshots";
      trackpad = {
        Clicking = true;
        Dragging = true;
      };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape = true;
    };
    startup.chime = false;
  };

  home-manager.users."charlotte.vanpetegem".home = {
    packages = with pkgs; [ docker-compose ];
    sessionPath = [ config.homebrew.brewPrefix ];
  };
}
