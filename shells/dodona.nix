{ pkgs, inputs }:
let
  support-services = {
    memcached.command = "memcached";
    mysql.command = "mysql";
    worker.command = "rails jobs:work";
    css.command = "yarn build:css --watch";
    js.command = "yarn build:js --watch";
  };
  all-services = support-services // {
    rails = {
      name = "server";
      command = "rails s -p 3000";
    };
  };
in
pkgs.devshell.mkShell {
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
        docker run --name dodona-db -p 3306:3306 --rm -v dodona-db-data:/var/lib/mysql -e MYSQL_ROOT_PASSWORD=dodona mariadb:latest
      '';
    }
  ];
  serviceGroups = {
    server.services = all-services;
    server-support.services = support-services;
  };
  language.c = {
    compiler = pkgs.gcc;
    includes = [ pkgs.libmysqlclient pkgs.zlib pkgs.libffi ];
    libraries = [ pkgs.libmysqlclient pkgs.zlib pkgs.libffi ];
  };
}
