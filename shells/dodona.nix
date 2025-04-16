{ pkgs, inputs, ... }:
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
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  packages = with pkgs; [
    chromedriver
    ungoogled-chromium
    nodejs
    nodePackages.typescript-language-server
    yarn
  ];
  env = [
    { name = "DATABASE_URL"; value = "trilogy://root:dodona@127.0.0.1:3306/dodona"; }
    { name = "TEST_DATABASE_URL"; value = "trilogy://root:dodona@127.0.0.1:3306/dodona_test"; }
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
  language.ruby = {
    package = pkgs.ruby_3_3;
    nativeDeps = [ pkgs.libmysqlclient pkgs.openssl pkgs.zlib pkgs.libffi pkgs.libyaml ];
  };
}
