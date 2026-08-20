{ devshell
, inputs
, azure-cli
, chromedriver
, libffi
, libmysqlclient
, libyaml
, nodejs_24
, openssl
, rsync
, ruby_4_0
, ungoogled-chromium
, zlib
}:

let
  support-services = {
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
devshell.mkShell {
  name = "Dodona";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  packages = [
    azure-cli
    chromedriver
    nodejs_24
    rsync
  ];
  env = [
    { name = "DATABASE_URL"; value = "trilogy://root:dodona@127.0.0.1:3306/dodona"; }
    { name = "CACHE_DATABASE_URL"; value = "trilogy://root:dodona@127.0.0.1:3306/dodona_cache"; }
    { name = "TEST_DATABASE_URL"; value = "trilogy://root:dodona@127.0.0.1:3306/dodona_test"; }
    { name = "NODE_ENV"; value = "development"; }
    { name = "PUPPETEER_EXECUTABLE_PATH"; value = "${ungoogled-chromium.outPath}/bin/chromium"; }
  ];
  commands = [
    {
      name = "deps:install";
      category = "[general commands]";
      help = "Install dependencies";
      command = ''
        bundle install
        npm install
      '';
    }
    {
      name = "deps:install:force";
      category = "[general commands]";
      help = "Install dependencies";
      command = ''
        bundle install
        bundle pristine
        npm install
      '';
    }
    {
      name = "git:delete-merged";
      category = "[general commands]";
      help = "Delete merged branches";
      command = ''
        git fetch -p ; git branch -r | awk '{print $1}' | egrep -v -f - <(git branch -vv | grep origin) | awk '{print $1}' | xargs -r git branch -D
      '';
    }
    {
      name = "lint:all";
      category = "[general commands]";
      help = "Run all linters in fix mode";
      command = "rubocop; npm run lint; npm run lint:css; erb_lint --lint-all;";
    }
    {
      name = "lint:all:fix";
      category = "[general commands]";
      help = "Run all linters in fix mode";
      command = ''
        herb format
        rubocop -a
        npm run lint -- --fix
        npm run lint:css -- --fix
        herb analyze
        herb lint --fix
      '';
    }
  ];
  serviceGroups = {
    server.services = all-services;
    server-support.services = support-services;
  };
  language.ruby = {
    package = ruby_4_0;
    nativeDeps = [ libmysqlclient openssl zlib libffi libyaml ];
  };
}
