{ pkgs, ... }: {
  hardware = {
    opengl = {
      driSupport32Bit = true;
      extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    };
    pulseaudio.support32Bit = true;
  };

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ pkgs.steam ];
  };
}
