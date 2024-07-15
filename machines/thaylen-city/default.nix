{ pkgs, ... }:

{
  homebrew = {
    enable = true;
    casks = [
      "docker"
      "libreoffice"
      "rubymine"
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
        sessionVariables = { DEFAULT_USER = "charlotte.vanpetegem"; };
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
      packages = [ pkgs.nix-direnv pkgs.docker-compose pkgs.autojump pkgs.tmux pkgs.ripgrep ];
      stateVersion = "24.11";
    };
  };
}
