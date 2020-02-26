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
            rev = "0.6.0";
            sha256 = "0zmq66dzasmr5pwribyh4kbkk23jxbpdw4rjxx0i7dx8jjp2lzl4";
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
