{ pkgs, inputs }:
let
  support-procfile-text = ''
    memcached: memcached
    mysql: mysql
    worker: rails jobs:work
    css: yarn build:css --watch
    js: yarn build:js --watch
  '';
  support-procfile = pkgs.writeText "Procfile.supp" support-procfile-text;
  all-procfile-text = support-procfile-text + ''
    server: rails s -p 3000
  '';
  all-procfile = pkgs.writeText "Procfile.all" all-procfile-text;

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
    {
      name = "server-support";
      category = "general commands";
      help = "Run everything required for a server";
      command = ''
        bundle install
        yarn install
        ${pkgs.honcho}/bin/honcho start -f ${support-procfile} -d $PRJ_ROOT
      '';
    }
    {
      name = "server";
      category = "general commands";
      help = "Run everything";
      command = ''
        bundle install
        yarn install
        ${pkgs.honcho}/bin/honcho start -f ${all-procfile} -d $PRJ_ROOT
      '';
    }
  ];
  language.c = {
    compiler = pkgs.gcc;
    includes = [ pkgs.libmysqlclient pkgs.zlib pkgs.libffi ];
    libraries = [ pkgs.libmysqlclient pkgs.zlib pkgs.libffi ];
  };
}
