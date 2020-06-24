{ pkgs, stdenv }:
let
  gemoji = pkgs.buildRubyGem {
    pname = "gemoji";
    gemName = "gemoji";
    source.sha256 = "1xv38sxql1fmaxi5lzj6v98l2aqhi6bqkhi6kqd0k38vw40l3yqc";
    type = "gem";
    version = "4.0.0.rc2";
  };
  emoji_list = stdenv.mkDerivation {
    name = "emoji_list";
    buildInputs = [ pkgs.ruby gemoji ];
    unpackPhase = "true";
    buildPhase = ''
      cat > extract_emoji.rb <<HERE
      require 'emoji'
      File.open('emoji_list.txt', 'w') do |f|
        Emoji.all.each do |e|
          f.puts("#{e.raw} #{e.description} #{e.name}#{(" " + e.tags.join(" ")) if e.tags.any?} (#{e.category})")
        end
      end
      HERE
      ruby extract_emoji.rb
    '';
    installPhase = ''
      cp emoji_list.txt $out
    '';
  };
in
pkgs.writeScriptBin "launcher" ''
  #!${pkgs.zsh}/bin/zsh

  _sighandler() {
    kill -INT "$child" 2>/dev/null
  }

  calc_options() {
    echo "calc "
  }

  calc() {
    if [ -n "$1" ]
    then
      ${pkgs.libqalculate}/bin/qalc "$1"
      sleep 5
    else
      ${pkgs.libqalculate}/bin/qalc
    fi
  }

  emoji_options() {
    cat ${emoji_list} | sed "s/^/emoji /"
  }

  emoji() {
    char=$(echo -n "$1" | sed "s/^\([^ ]*\) .*/\1/")
    ${pkgs.sway}/bin/swaymsg exec -- "echo -n $char | ${pkgs.wl-clipboard}/bin/wl-copy --foreground"
  }

  pass_options(){
    prefix=''${PASSWORD_STORE_DIR-~/.password-store}
    password_files=( "$prefix"/**/*.gpg )
    printf 'password %s\n' ''${''${password_files%.gpg}#$prefix/}
    printf 'username %s\n' ''${''${password_files%.gpg}#$prefix/}
    printf 'otp %s\n' ''${''${password_files%.gpg}#$prefix/}
  }

  username() {
    swaymsg exec -- "${pkgs.pass}/bin/pass show '$1' | sed -n 's/^Username: *//p' | tr -d '\n' | ${pkgs.wl-clipboard}/bin/wl-copy --foreground"
  }
  password() {
    swaymsg exec -- "${pkgs.pass}/bin/pass show -c0 '$1'"
  }
  otp() {
    swaymsg exec -- "${pkgs.pass}/bin/pass otp -c '$1'"
  }

  record_options() {
    ${pkgs.sway}/bin/swaymsg -t get_outputs | ${pkgs.jq}/bin/jq -r '.[]["name"]' | sed "s/^/record /"
    echo record select
  }

  record() {
    filename="$(${pkgs.xdg-user-dirs}/bin/xdg-user-dir VIDEOS)/$(date +'screenrecording_%y-%m-%d-%H%M%S.mp4')"

    trap _sighandler SIGINT
    if [[ "$1" = "select" ]]
    then
      ${pkgs.wf-recorder}/bin/wf-recorder -g "$(${pkgs.slurp}/bin/slurp)" -f "$filename" &
    else
      wf-recorder -o $! -f "$filename" &
    fi
    child=$!
    wait "$child"
    # We wait two times, because the first wait exits when the process receives a signal. The process might have finished though, so we ignore errors.
    wait "$child" 2>/dev/null
    if [ -f "$filename" ]
    then
      echo "Saved as $filename"
    else
      echo "Something went wrong while recording"
    fi
    sleep 5
  }

  run_options() {
    print -rl -- ''${(ko)commands} | sed "s/^/run /"
  }

  run() {
    ${pkgs.sway}/bin/swaymsg exec $1
  }

  ssh_options() {
    cat $HOME/.ssh/config | grep "^Host [a-zA-Z]\+" | sed "s/Host /ssh /"
  }

  ssh() {
    ${pkgs.sway}/bin/swaymsg exec "${pkgs.kitty}/bin/kitty -e ssh $1"
  }

  systemctl_options() {
    echo systemctl hibernate
    echo systemctl poweroff
    echo systemctl reboot
    echo systemctl suspend
  }

  tmuxinator_options() {
    ls ~/.config/tmuxinator | sed "s/\.yml$//" | sed "s/^/tmuxinator /"
  }

  tmuxinator() {
    ${pkgs.sway}/bin/swaymsg exec "${pkgs.kitty}/bin/kitty -e ${pkgs.tmuxinator}/bin/tmuxinator start $1"
  }

  windows_options() {
    ${pkgs.sway}/bin/swaymsg -t get_tree | ${pkgs.jq}/bin/jq -r 'recurse(.nodes[]?)|recurse(.floating_nodes[]?)|select(.layout=="none")|select(.app_id!="launcher")|select(.type=="con"),select(.type=="floating_con")|(if .app_id then .app_id else .window_properties.class end)+": "+.name+" ("+(.id|tostring)+")"' | sed "s/^/windows /"
  }

  windows() {
    window=$(echo $@ | sed 's/.* (\([^)]*\))$/\1/')
    ${pkgs.sway}/bin/swaymsg \[con_id="$window"\] focus
  }

  CHOSEN=$(cat <(windows_options) <(tmuxinator_options) <(ssh_options) <(systemctl_options) <(pass_options) <(run_options) <(record_options) <(calc_options) <(emoji_options) | ${pkgs.fzy}/bin/fzy --lines 36 | tail -n1)

  if [ -n "$CHOSEN" ]
  then
    PREFIX=$(echo $CHOSEN | sed "s/^\([^ ]*\) .*/\1/g")
    WORD=$(echo $CHOSEN | sed "s/^[^ ]* \(.*\)/\1/g")

    $PREFIX $WORD
  fi
''
