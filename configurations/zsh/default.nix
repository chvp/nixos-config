{ ... }:

{
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
      initExtra = ''
        source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh
        ${pkgs.any-nix-shell}/bin/any-nix-shell zsh --info-right | source /dev/stdin
      '';
      oh-my-zsh = {
        enable = true;
        plugins = [
          "autojump"
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
        PASSWORD_STORE_DIR = "$HOME/repos/passwords";
      };
      shellAliases = {
        upgrade = "sudo nix-channel --update && sudo nixos-rebuild switch";
      };
    };
  };
}
