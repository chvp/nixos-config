{ pkgs, inputs }: pkgs.devshell.mkShell {
  name = "Gamification 2";
  imports = [ "${inputs.devshell}/extra/language/c.nix" ];
  packages = with pkgs; [
    (pkgs.lowPrio binutils)
    findutils
    cmake
    gnumake
    nodejs
    postgresql_14
    ruby_3_1
    yarn
  ];
  env = [
    { name = "PGDATA"; eval = "$PRJ_DATA_DIR/postgres"; }
    { name = "DATABASE_HOST"; eval = "$PGDATA"; }
    { name = "GEM_HOME"; eval = "$PRJ_DATA_DIR/bundle/$(ruby -e 'puts RUBY_VERSION')"; }
    { name = "PATH"; prefix = "$GEM_HOME/bin"; }
  ];
  commands = [
    {
      name = "pg:setup";
      category = "database";
      help = "Setup postgres in project folder";
      command = ''
        initdb --encoding=UTF8 --no-locale --no-instructions -U postgres
        echo "listen_addresses = ${"'"}${"'"}" >> $PGDATA/postgresql.conf
        echo "unix_socket_directories = '$PGDATA'" >> $PGDATA/postgresql.conf
        echo "CREATE USER gamification2 WITH PASSWORD 'gamification2' CREATEDB SUPERUSER;" | postgres --single -E postgres
      '';
    }
    {
      name = "pg:start";
      category = "database";
      help = "Start postgres instance";
      command = ''
        [ ! -d $PGDATA ] && pg:setup
        pg_ctl -D $PGDATA -U postgres start -l log/postgres.log
      '';
    }
    {
      name = "pg:stop";
      category = "database";
      help = "Stop postgres instance";
      command = ''
        pg_ctl -D $PGDATA -U postgres stop
      '';
    }
    {
      name = "pg:console";
      category = "database";
      help = "Open database console";
      command = ''
        psql --host $PGDATA -U postgres
      '';
    }
  ];
  language.c = {
    compiler = pkgs.gcc;
    includes = [ pkgs.zlib pkgs.openssl ];
    libraries = [ pkgs.zlib pkgs.openssl ];
  };
}
