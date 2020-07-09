{ ... }:

{
  custom.zfs.homeLinks = [
    { path = ".config/0ad"; type = "cache"; }
  ];

  nixpkgs.overlays = [
    (self: super: {
      zeroadPackages = super.zeroadPackages.override { newScope = extra: self.newScope ({ stdenv = self.stdenvAdapters.impureUseNativeOptimizations self.stdenv; } // extra); };
    })
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ pkgs.zeroad ];
  };
}
