let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    buildInputs = [
      pkgs.ffmpeg
      pkgs.postgresql
      pkgs.ruby_2_7
      pkgs.taglib
      pkgs.zlib
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
    '';
  }
