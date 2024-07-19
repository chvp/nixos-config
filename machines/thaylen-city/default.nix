{ pkgs, ... }:

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

  homebrew = {
    enable = true;
    casks = [
      "docker"
      "inkscape"
      "libreoffice"
      "rubymine"
      "tsh"
    ];
    global.brewfile = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };
  };
  security.pam.enableSudoTouchIdAuth = true;
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

  home-manager.users."charlotte.vanpetegem".home.packages = with pkgs; [ docker-compose ];
}
