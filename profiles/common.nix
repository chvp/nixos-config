{ pkgs, ... }:

{
  imports = [
    <home-manager/nixos>
    ../modules/zfs.nix
    ../overlays/default.nix
    ../configurations/direnv.nix
    ../configurations/git.nix
    ../configurations/locale.nix
    ../configurations/neovim.nix
    ../configurations/nix-index.nix
    ../configurations/nix-store.nix
    ../configurations/ssh.nix
    ../configurations/tmux.nix
    ../configurations/users.nix
    ../configurations/zsh.nix
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [
      htop
      moreutils
      ncdu
      pass
      ripgrep
      unzip
      youtube-dl
    ];
  };

}
