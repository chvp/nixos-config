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

        trap "docker stop dodona-db" 0
        docker run --name dodona-db -p 3306:3306 --rm -v $(git rev-parse --show-toplevel)/tmp/db:/var/lib/mysql -e MYSQL_ROOT_PASSWORD=dodona mariadb:latest &

        child=$!
        wait $child
      '')
    ];
    shellHook = ''
      export DATABASE_URL="mysql2://root:dodona@127.0.0.1:3306/dodona"
    '';
  }
