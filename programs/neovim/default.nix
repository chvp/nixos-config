with import <nixpkgs> {};

let
  nixpkgs-master = import <nixpkgs-master> {};
in
  {
    home-manager.users.charlotte = { pkgs, ... }: {
      nixpkgs.overlays = [
        (self: super: {
          neovim = nixpkgs-master.neovim.override {
            configure = (import ../direnv/shells/vim-base.nix { pkgs = self; }) ;
          };
        })
      ];
      home.packages = [ pkgs.neovim ];
    };
  }
