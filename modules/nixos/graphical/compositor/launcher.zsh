#!@zsh@/bin/zsh

_sighandler() {
  kill -INT "$child" 2>/dev/null
}

calc_options() {
  echo "calc "
}

calc() {
  if [ -n "$1" ]
  then
    @libqalculate@/bin/qalc "$1"
    sleep 5
  else
    @libqalculate@/bin/qalc
  fi
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

systemctl_options() {
  echo systemctl hibernate
  echo systemctl poweroff
  echo systemctl reboot
  echo systemctl suspend
}

CHOSEN=$(cat <(systemctl_options) <(nrun_options) <(run_options) <(calc_options) <(emoji_options) | @fzy@/bin/fzy --lines 80 | tail -n1)

if [ -n "$CHOSEN" ]
then
  PREFIX=$(echo $CHOSEN | sed "s/^\([^ ]*\) .*/\1/g")
  WORD=$(echo $CHOSEN | sed "s/^[^ ]* \(.*\)/\1/g")

  $PREFIX $WORD
fi
