{ pkgs }:

pkgs.writeShellScriptBin "color_picker" ''
  color=$(${pkgs.grim}/bin/grim -t png -g "$(${pkgs.slurp}/bin/slurp -p)" - | ${pkgs.imagemagick}/bin/convert png:- -unique-colors txt:- | grep -o '#[A-F0-9]\+')

  ${pkgs.sway}/bin/swaymsg exec -- "echo -n '$color' | ${pkgs.wl-clipboard}/bin/wl-copy --foreground"
''
