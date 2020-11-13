{ pkgs, ... }: {
  hardware = {
    opengl = {
      driSupport32Bit = true;
      extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    };
    pulseaudio.support32Bit = true;
  };

  chvp.zfs.homeLinks = [
    { path = ".paradoxlauncher"; type = "data"; }
    { path = ".steam"; type = "data"; }
    { path = ".local/share/Steam"; type = "data"; }
    { path = ".local/share/Paradox Interactive"; type = "data"; }
  ];

  home-manager.users.charlotte = { pkgs, ... }: {
    home.packages = [ pkgs.steam ];
  };
}
