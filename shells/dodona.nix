{ pkgs, inputs }: pkgs.devshell.mkShell {
  name = "Dodona";
  imports = [ "${inputs.devshell}/extra/language/c.nix" ];
  packages = with pkgs; [
    (pkgs.lowPrio binutils)
    chromedriver
    ungoogled-chromium
    findutils
    gnumake
    nodejs
    nodePackages.typescript-language-server
    ruby_3_1
    rubyPackages_3_1.solargraph
    yarn
  ];
  env = [
    { name = "CC"; value = "cc"; }
    { name = "CPP"; value = "cpp"; }
    { name = "CXX"; value = "c++"; }
    { name = "DATABASE_URL"; value = "mysql2://root:dodona@127.0.0.1:3306/dodona"; }
    { name = "TEST_DATABASE_URL"; value = "mysql2://root:dodona@127.0.0.1:3306/dodona_test"; }
    { name = "GEM_HOME"; eval = "$PRJ_DATA_DIR/bundle/$(ruby -e 'puts RUBY_VERSION')"; }
    { name = "PATH"; prefix = "$GEM_HOME/bin"; }
  ];
  commands = [
    {
      name = "memcached";
      category = "general commands";
      help = "Start caching server";
      package = pkgs.memcached;
    }
    {
      name = "mysql";
      category = "general commands";
      help = "Start mysql (in docker container)";
      command = ''
        trap "systemd-run --user --no-block docker stop dodona-db" 0
        docker run -d --name dodona-db -p 3306:3306 --rm -v dodona-db-data:/var/lib/mysql -e MYSQL_ROOT_PASSWORD=dodona mariadb:latest
        while [ 1 -eq 1 ]
        do
          sleep 1000
        done
      '';
    }
    {
      name = "server-support";
      category = "general commands";
      help = "Run everything required for a server";
      command = ''
        memcached &
        trap "kill $!" 0
        mysql &
        trap "kill $!" 0
        bundle install
        yarn install
        rails jobs:work &
        trap "kill $!" 0
        yarn build:css --watch &
        trap "kill $!" 0
        yarn build:js --watch &
        trap "kill $!" 0
        wait $!
      '';
    }
    {
      name = "server";
      category = "general commands";
      help = "Run everything";
      command = ''
        memcached &
        trap "kill $!" 0
        mysql &
        trap "kill $!" 0
        bundle install
        yarn install
        rails jobs:work &
        trap "kill $!" 0
        yarn build:css --watch &
        trap "kill $!" 0
        yarn build:js --watch &
        trap "kill $!" 0
        rails s &
        trap "kill $!" 0
        wait $!
      '';
    }
  ];
  language.c = {
    compiler = pkgs.gcc;
    includes = [ pkgs.libmysqlclient pkgs.zlib pkgs.libffi ];
    libraries = [ pkgs.libmysqlclient pkgs.zlib pkgs.libffi ];
  };
}
