{ pkgs, ... }:

{
  chvp = {
    homeStateVersion = "24.11";
    systemStateVersion = 4;
    username = "charlotte.vanpetegem";
  };
  networking.computerName = "Thaylen City";

  homebrew = {
    enable = true;
    casks = [
      "docker"
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

  home-manager.users."charlotte.vanpetegem" = {
    programs = {
      # Until I get emacs set up on this machine
      zsh.sessionVariables.EDITOR = "vim";
      git = {
        enable = true;
        extraConfig = {
          branch.autosetuprebase = "always";
          github.user = "chvp";
          merge.conflictStyle = "diff3";
          pull.rebase = true;
          rerere.enabled = true;
          rebase.autoStash = true;
        };
        ignores = [
          ".DS_Store"
          ".data"
          ".direnv"
          ".envrc"
          ".idea"
          ".dir-locals.el"
        ];
        userEmail = "charlotte.vanpetegem@silverfin.com";
        userName = "Charlotte Van Petegem";
      };
    };

    home = {
      packages = with pkgs; [
        docker-compose
        tmux
      ];
    };
  };
}
