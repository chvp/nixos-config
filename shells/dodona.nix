{ pkgs, inputs, ... }:
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
pkgs.devshell.mkShell {
  name = "Dodona";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  devshell = {
    motd = "";
    startup = {
      # Hack to make sure Rubymine doesn't use an ephemeral path from the nix store
      "link-devshell-dir".text = ''
        ln -snf $DEVSHELL_DIR $PRJ_DATA_DIR/devshell
      '';
    };
  };
  packages = with pkgs; [
    chromedriver
    ungoogled-chromium
    nodejs_22
    nodePackages.typescript-language-server
    rsync
  ];
  env = [
    { name = "DATABASE_URL"; value = "trilogy://root:dodona@127.0.0.1:3306/dodona"; }
    { name = "CACHE_DATABASE_URL"; value = "trilogy://root:dodona@127.0.0.1:3306/dodona_cache"; }
    { name = "TEST_DATABASE_URL"; value = "trilogy://root:dodona@127.0.0.1:3306/dodona_test"; }
    { name = "NODE_ENV"; value = "development"; }
    { name = "PUPPETEER_EXECUTABLE_PATH"; value = "${pkgs.ungoogled-chromium.outPath}/bin/chromium"; }
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
      command = "rubocop -a; npm run lint --fix; npm run lint:css --fix; erb_lint --lint-all -a;";
    }
  ];
  serviceGroups = {
    server.services = all-services;
    server-support.services = support-services;
  };
  language.ruby = {
    package = pkgs.ruby_3_4;
    nativeDeps = [ pkgs.libmysqlclient pkgs.openssl pkgs.zlib pkgs.libffi pkgs.libyaml ];
  };
}
