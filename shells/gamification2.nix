{ pkgs, inputs, ... }: pkgs.devshell.mkShell {
  name = "Gamification 2";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  env = [
    { name = "PGDATA"; eval = "$PRJ_DATA_DIR/postgres"; }
    { name = "DATABASE_HOST"; eval = "$PGDATA"; }
  ];
  commands = [
    {
      name = "refresh-deps";
      category = "general commands";
      help = "Install dependencies";
      command = ''
        yarn install
        bundle install
        bundle pristine
      '';
    }
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
        postgres
      '';
    }
    {
      name = "pg:console";
      category = "database";
      help = "Open database console";
      command = ''
        psql --host $PGDATA s
      '';
    }
  ];
  packages = with pkgs; [
    cmake
    nodejs
    (lib.lowPrio postgresql_14)
    yarn
  ];
  serviceGroups.server.services = {
    web.command = "rails s -p 3000";
    postgres.command = "pg:start";
  };
  language.ruby = {
    package = pkgs.ruby_3_3;
    nativeDeps = [ pkgs.postgresql_14 pkgs.zlib pkgs.openssl pkgs.libyaml ];
  };
}
