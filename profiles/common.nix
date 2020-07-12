{ pkgs, ... }:
let
  findImport = (import ../lib.nix).findImport;
  home-manager = findImport "home-manager";
in
{
  imports = [
    "${home-manager}/nixos"
    ../modules/zfs.nix
    ../configurations/direnv.nix
    ../configurations/git.nix
    ../configurations/gnupg.nix
    ../configurations/locale.nix
    ../configurations/neovim.nix
    ../configurations/nix-index.nix
    ../configurations/nix-store.nix
    ../configurations/pass.nix
    ../configurations/ssh.nix
    ../configurations/tmux.nix
    ../configurations/users.nix
    ../configurations/zsh.nix
  ];

  home-manager.useGlobalPkgs = true;
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [
      hledger
      htop
      moreutils
      ncdu
      pandoc
      ripgrep
      texlive.combined.scheme-small
      unzip
      youtube-dl
    ];
  };
}
