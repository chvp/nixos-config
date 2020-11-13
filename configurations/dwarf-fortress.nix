{ ... }:

{
  chvp.zfs.homeLinks = [
    { type = "data"; path = ".local/share/df_linux"; }
  ];

  nixpkgs.overlays = [
    (self: super: {
      dwarf-fortress = super.dwarf-fortress.override {
        enableSound = false;
      };
    })
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = with pkgs; [ dwarf-fortress ];
  };
}
