let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      ruby
      yarn
      nodejs-12_x
      libmysqlclient
      zlib
      (pkgs.writeScriptBin "start-db" ''
        #!${pkgs.zsh}/bin/zsh

        _sighandler() {
          docker stop dodona-db
        }

        trap _sighandler SIGINT
        trap _sighandler SIGTERM
        trap _sighandler SIGHUP

        docker run --name dodona-db -p 3306:3306 --rm -v $(git rev-parse --show-toplevel)/tmp/db:/var/lib/mysql -e MYSQL_ROOT_PASSWORD=dodona mariadb:latest &

        child=$!
        wait $child
        # We wait two times, because the first wait exits when the process receives a signal. The process might have finished though, so we ignore errors.
        wait $child 2>/dev/null
      '')
    ];
    shellHook = ''
      export DATABASE_URL="mysql2://root:dodona@127.0.0.1:3306/dodona"
    '';
  }
