{ ... }:

{
  imports = [ ./base-x.nix ];
  home-manager.users.charlotte = { pkgs, ... }: {
    xdg.configFile."openbox/rc.xml".source = ./openbox/rc.xml;
    xsession = {
      windowManager.command = "${pkgs.openbox}/bin/openbox";
      initExtra = ''
        ${pkgs.tint2}/bin/tint2 &
      '';
    };
  };
}
