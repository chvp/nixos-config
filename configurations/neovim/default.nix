with import <nixpkgs> { };

{
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [
      (
        pkgs.neovim.override {
          configure = (import ./base.nix { inherit pkgs; });
        }
      )
    ];
  };
}
