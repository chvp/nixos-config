let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    chromedriver
    libmysqlclient
    nodejs-12_x
    ruby
    yarn
    zlib
    (
      pkgs.writeScriptBin "start-dockers" ''
        #!${bash}/bin/bash

        function stopdockers {
          echo ${docker}/bin/docker stop dodona-db | ${at}/bin/at NOW
          echo ${docker}/bin/docker stop dodona-cache | ${at}/bin/at NOW
        }

        trap stopdockers 0

        docker run -d --name dodona-db -p 3306:3306 --rm -v dodona-db-data:/var/lib/mysql -e MYSQL_ROOT_PASSWORD=dodona mariadb:latest
        docker run -d --name dodona-cache -p 11211:11211 --rm memcached:latest

        while [ 1 -eq 1 ]
        do
          sleep 1000
        done
      ''
    )
  ];
  shellHook = ''
    export DATABASE_URL="mysql2://root:dodona@127.0.0.1:3306/dodona"
    export GEM_HOME="$PWD/vendor/rubygems/$(ruby -e 'puts RUBY_VERSION')"
    export PATH="$GEM_HOME/bin:$PATH"
  '';
}
