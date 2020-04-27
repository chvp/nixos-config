with import <nixpkgs> { };
{ ... }:
let
  launcher = import ../sway/launcher.nix { inherit pkgs stdenv; };
  status-configuration = import ../sway/status-configuration.nix { inherit pkgs; };
in
{
  imports = [ ../base-x/default.nix ];
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
              fonts = [ "Fira Code Normal 9" ];
              position = "top";
              statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ${status-configuration}";
            }
          ];
          floating.criteria = [{ class = "launcher"; } { class = "accentor.Main"; }];
          fonts = [ "Fira Code Normal 9" ];
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
}
