let
  pkgs = import <nixpkgs> {};
  baseVimConfig = import ../programs/neovim/base.nix { inherit pkgs; };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    ffmpeg
    postgresql
    ruby_2_7
    taglib
    zlib
    (
      neovim.override {
        configure = {
          customRC = baseVimConfig.customRC + ''
            " Required for operations modifying multiple buffers like rename
            set hidden

            let g:LanguageClient_serverCommands = {
            \ 'ruby': ['${solargraph}/bin/solargraph', 'stdio'],
            \ }
          '';
          vam.knownPlugins = baseVimConfig.vam.knownPlugins;
          vam.pluginDictionaries = (baseVimConfig.vam.pluginDictionaries or []) ++ [
            {
              names = [
                "LanguageClient-neovim"
                "vim-ruby"
              ];
            }
          ];
        };
      }
    )
  ];
  shellHook = ''
    export PGDATA=$PWD/tmp/postgres_data
    export PGHOST=$PWD/tmp/postgres
    export PGDATABASE=postgres
    export DATABASE_URL="postgresql:///postgres?host=$PGHOST"
    if [ ! -d $PGHOST ]; then
      mkdir -p $PGHOST
    fi
    if [ ! -d $PGDATA ]; then
      echo 'Initializing postgresql database...'
      initdb $PGDATA --auth=trust >/dev/null
    fi
    cat >"$PGDATA/postgresql.conf" <<HERE
      listen_addresses = '''
      unix_socket_directories = '$PGHOST'
    HERE
    export GEM_HOME="$PWD/vendor/rubygems"
    export PATH="$GEM_HOME/bin:$PATH"
  '';
}
