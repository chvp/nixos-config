{ pkgs, ... }:

{
  imports = [
    ../modules/zfs.nix
    ../configurations/direnv.nix
    ../configurations/gnupg.nix
    ../configurations/hledger.nix
    ../configurations/locale.nix
    ../configurations/mail.nix
    ../configurations/neovim.nix
    ../configurations/nix-index.nix
    ../configurations/nix-store.nix
    ../configurations/pass.nix
    ../configurations/tmux.nix
    ../configurations/users.nix
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [
      moreutils
      pandoc
      texlive.combined.scheme-small
      unzip
      youtube-dl
    ];
  };
}
