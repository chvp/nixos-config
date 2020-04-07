{ ... }:

{
  programs.command-not-found.enable = true;

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ pkgs.autojump ];
    programs.zsh = {
      enable = true;
      enableAutosuggestions = true;
      autocd = true;
      dotDir = ".config/zsh";
      history = {
        expireDuplicatesFirst = true;
        path = "\$HOME/.config/zsh/zsh_history";
      };
      oh-my-zsh = {
        enable = true;
        plugins = [
          "autojump"
          "command-not-found"
          "common-aliases"
          "extract"
          "history-substring-search"
          "git"
          "sudo"
          "systemd"
          "tmux"
        ];
        theme = "agnoster";
      };
      plugins = [
        {
          name = "zsh-syntax-highlighting";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-syntax-highlighting";
            rev = "0.7.1";
            sha256 = "03r6hpb5fy4yaakqm3lbf4xcvd408r44jgpv4lnzl9asp4sb9qc0";
          };
        }
      ];
      sessionVariables = {
        DEFAULT_USER = "charlotte";
        EDITOR = "nvim";
      };
      shellAliases = {
        upgrade = "sudo nix-channel --update && sudo nixos-rebuild switch";
      };
    };
  };
}
