{ pkgs }:

pkgs.writeScriptBin "screenshot" ''
  #!${pkgs.zsh}/bin/zsh

  while getopts ":rd" opt
  do
    case "''${opt}" in
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
    name=$(${pkgs.utillinux}/bin/uuidgen).png
    ${pkgs.grim}/bin/grim -t png -g "$dims" - | ${pkgs.openssh}/bin/ssh sunspear "cat > /usr/share/nginx/html/screenshots/$name"
    path="https://cvpetegem.be/screenshots/$name"
  else
    name=$(date +'screenshot_%Y-%m-%d-%H%M%S.png')
    path="$(${pkgs.xdg-user-dirs}/bin/xdg-user-dir PICTURES)/$name"
    ${pkgs.grim}/bin/grim -g "$dims" "$path"
  fi

  ${pkgs.sway}/bin/swaymsg exec -- "echo -n '$path' | ${pkgs.wl-clipboard}/bin/wl-copy --foreground"
  ${pkgs.libnotify}/bin/notify-send "Screenshot taken" "$path"
''
