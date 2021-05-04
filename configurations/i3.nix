{ config, pkgs, ... }:
let
  launcher = import ./sway/launcher.nix { inherit pkgs; stdenv = pkgs.stdenv; };
  status-configuration = import ./sway/status-configuration.nix { inherit pkgs config; };
in
{
  imports = [ ./base-x.nix ];

  config = {
    home-manager.users.charlotte = { pkgs, ... }: {
      xsession = {
        windowManager.i3 = {
          enable = true;
          config = {
            bars = [
              {
                colors = {
                  background = "#fbffff";
                  statusline = "#535c65";
                  focusedWorkspace = { background = "#2b7ab2"; border = "#2b7ab2"; text = "#fbffff"; };
                  activeWorkspace = { background = "#6d7782"; border = "#6d7782"; text = "#fbffff"; };
                  inactiveWorkspace = { background = "#fbffff"; border = "#fbffff"; text = "535c65"; };
                  urgentWorkspace = { background = "#ae5865"; border = "#ae5865"; text = "#fbffff"; };
                };
                fonts = { names = [ "Fira Code" ]; size = 9.0; style = "Normal"; };
                position = "top";
                statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ${status-configuration}";
              }
            ];
            floating.criteria = [{ class = "launcher"; } { class = "accentor.Main"; }];
            fonts = { names = [ "Fira Code" ]; size = 9.0; style = "Normal"; };
            menu = "${pkgs.kitty}/bin/kitty --class launcher -e ${launcher}/bin/launcher";
            modifier = "Mod4";
            terminal = "${pkgs.kitty}/bin/kitty";
          };
          extraConfig = ''
            default_border pixel
          '';
        };
      };
    };
  };
}
