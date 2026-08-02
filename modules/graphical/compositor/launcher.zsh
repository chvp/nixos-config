#!@zsh@/bin/zsh

_sighandler() {
  kill -INT "$child" 2>/dev/null
}

emoji_options() {
  @uni@/bin/uni emoji all | sed "s/^/emoji /"
}

emoji() {
  char=$(echo -n "$1" | sed "s/^\([^ ]*\) .*/\1/")
  riverctl spawn "echo -n $char | @wl-clipboard@/bin/wl-copy --foreground"
}

nrun_options() {
    echo "nrun "
}

nrun() {
   riverctl spawn "@nix@/bin/nix run nixpkgs\#$1"
}

run_options() {
  print -rl -- ''${(ko)commands} | grep -v "^\\." | sed "s/^/run /"
}

run() {
  riverctl spawn $1
}

trun_options() {
  run_options | sed "s/^/t/"
}

trun() {
    riverctl spawn "@foot@/bin/foot zsh -ic $1"
}

systemctl_options() {
  echo systemctl poweroff
  echo systemctl reboot
  echo systemctl suspend
  echo systemctl hibernate
}

if [[ "$(@darkman@/bin/darkman get)" == "light" ]]
then
  export BEMENU_OPTS='--fb "#eff1f5" --ff "#4c4f69" --nb "#eff1f5" --nf "#4c4f69" --tb "#eff1f5" --hb "#eff1f5" --tf "#d20f39" --hf "#df8e1d" --af "#4c4f69" --ab "#eff1f5"'
else
  export BEMENU_OPTS='--fb "#303446" --ff "#c6d0f5" --nb "#303446" --nf "#c6d0f5" --tb "#303446" --hb "#303446" --tf "#e78284" --hf "#e5c890" --af "#c6d0f5" --ab "#303446"'
fi

CHOSEN=$(cat <(systemctl_options) <(nrun_options) <(run_options) <(trun_options) <(emoji_options) | @bemenu@/bin/bemenu -w -l 40 -p select -n --counter always -P '*')

if [ -n "$CHOSEN" ]
then
  PREFIX=$(echo $CHOSEN | sed "s/^\([^ ]*\) .*/\1/g")
  WORD=$(echo $CHOSEN | sed "s/^[^ ]* \(.*\)/\1/g")

  $PREFIX $WORD
fi
