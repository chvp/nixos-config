{ pkgs, ... }:

{
  homebrew = {
    enable = true;
    brews = [
    ];
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
  networking = {
    computerName = "Thaylen City";
    hostName = "thaylen-city";
  };
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
    configureBuildUsers = true;
  };
  programs = {
    bash.enable = false;
    zsh.enable = true;
  };
  security.pam.enableSudoTouchIdAuth = true;
  services.nix-daemon.enable = true;
  users.users."charlotte.vanpetegem" = {
    name = "charlotte.vanpetegem";
    home = "/Users/charlotte.vanpetegem";
    shell = pkgs.zsh;
  };
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
    stateVersion = 4;
  };

  home-manager.users."charlotte.vanpetegem" = {
    programs = {
      direnv = {
        enable = true;
        enableZshIntegration = true;
        nix-direnv = {
          enable = true;
        };
        config = {
          global = {
            load_dotenv = true;
          };
        };
      };
      zsh = {
        enable = true;
        autocd = true;
        autosuggestion.enable = true;
        dotDir = ".config/zsh";
        history = {
          expireDuplicatesFirst = true;
          path = "$HOME/.local/share/zsh/history";
        };
        oh-my-zsh = {
          enable = true;
          plugins = [
            "autojump"
            "common-aliases"
            "extract"
            "history-substring-search"
            "git"
            "tmux"
          ];
          theme = "robbyrussell";
        };
        sessionVariables = { 
          DEFAULT_USER = "charlotte.vanpetegem"; 
          # Until I get emacs set up on this machine
	  EDITOR = "vim";
        };
        shellAliases = {
          gupd = "gfa && gprom";
        };
        syntaxHighlighting.enable = true;
      };
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
        autojump
        coreutils
        docker-compose
        nix-direnv
        ripgrep
        tmux
      ];
      stateVersion = "24.11";
    };
  };
}
