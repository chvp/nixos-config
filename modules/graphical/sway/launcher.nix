{ pkgs, stdenv }:
let
  gemoji = pkgs.buildRubyGem {
    pname = "gemoji";
    gemName = "gemoji";
    source.sha256 = "1xv38sxql1fmaxi5lzj6v98l2aqhi6bqkhi6kqd0k38vw40l3yqc";
    type = "gem";
    version = "4.0.0.rc2";
  };
  emojiList = stdenv.mkDerivation {
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
  script = pkgs.substituteAll {
    src = ./launcher.zsh;
    inherit (pkgs)
      fzy
      jq
      kitty
      libqalculate
      pass
      slurp
      sway
      zsh
      ;
    inherit emojiList;
    wfRecorder = pkgs.wf-recorder;
    wlClipboard = pkgs.wl-clipboard;
    xdgUserDirs = pkgs.xdg-user-dirs;
  };
in
pkgs.runCommandNoCC "launcher" { } ''
  mkdir -p $out/bin
  cp ${script} $out/bin/launcher
  chmod +x $out/bin/launcher
''
