{ ... }:
{
  services.xserver = {
    enable = true;
    displayManager.startx.enable = true;
    libinput = {
      enable = true;
      disableWhileTyping = true;
      scrollMethod = "twofinger";
      tapping = true;
      tappingDragLock = true;
    };
    xkbVariant = "altgr-intl";
  };
  home-manager.users.charlotte = { pkgs, ... }: {
    home.file.".xinitrc".text = "source ~/.xsession";
    xsession = {
      enable = true;
      numlock.enable = true;

    };
  };
}
