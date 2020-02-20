with import <nixpkgs> {};

{
  home-manager.users.charlotte = { pkgs, ... }: {
    nixpkgs.overlays = [
      (self: super: {
        neovim = super.neovim.override {
          configure = (import ../direnv/shells/vim-base.nix { pkgs = self; }) ;
        };
      })
    ];
    home.packages = [ pkgs.neovim ];
  };
}
