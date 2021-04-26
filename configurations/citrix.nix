{ ... }: {
  chvp = {
    nix.unfreePackages = [ "citrix-workspace" ];
    zfs.homeLinks = [
      { path = ".ICAClient"; type = "data"; }
    ];
  };
  nixpkgs.overlays = [
    (self: super: {
      citrix_workspace = super.citrix_workspace.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [ self.libdrm self.mesa ];
      });
    })
  ];
  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ citrix_workspace ];
  };
}
