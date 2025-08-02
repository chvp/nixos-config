{ pkgs }:

pkgs.writeShellScriptBin "screenshot" ''
  while getopts ":dfr" opt
  do
    case "''${opt}" in
      f)
        file=true
        ;;
      r)
        remote=true
        ;;
      d)
        delay=true
        ;;
    esac
  done

  dims="$(${pkgs.slurp}/bin/slurp)"

  if [[ -n "$delay" ]]
  then
    sleep 5
  fi

  if [[ -n "$remote" ]]
  then
    name=$(${pkgs.util-linux}/bin/uuidgen).png
    ${pkgs.grim}/bin/grim -t png -g "$dims" - | ${pkgs.openssh}/bin/ssh data "cat > data/public/$name"
    path="https://data.vanpetegem.me/public/$name"
  elif [[ -n "$file" ]]
  then
    name=$(date +'screenshot_%Y-%m-%d-%H%M%S.png')
    path="$(${pkgs.xdg-user-dirs}/bin/xdg-user-dir PICTURES)/$name"
    ${pkgs.grim}/bin/grim -g "$dims" "$path"
  else
    ${pkgs.grim}/bin/grim -g "$dims" -t png - | ${pkgs.wl-clipboard}/bin/wl-copy -t image/png
  fi

  if [[ -n "$path" ]]
  then
    echo -n "$path" | ${pkgs.wl-clipboard}/bin/wl-copy
    ${pkgs.libnotify}/bin/notify-send "Screenshot taken" "$path"
  else
    ${pkgs.libnotify}/bin/notify-send "Screenshot taken"
  fi

''
