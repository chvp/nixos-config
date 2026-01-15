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
    (azure-cli.withExtensions [ azure-cli-extensions.monitor-control-service ])
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
      name = "refresh-deps";
      category = "[general commands]";
      help = "Install dependencies";
      command = ''
        yarn install
        bundle install
        bundle pristine
      '';
    }
    {
      name = "mysql";
      category = "[general commands]";
      help = "Start mysql (in docker container)";
      command = ''
        docker run --name dodona-db -p 3306:3306 --rm -v dodona-db-data:/var/lib/mysql -e MYSQL_ROOT_PASSWORD=dodona mariadb:latest
      '';
    }
    {
      name = "dbshell";
      category = "[general commands]";
      help = "Attach a client to the db";
      command = ''
        docker exec -it dodona-db mariadb --user=root --password=dodona dodona
      '';
    }
    {
      name = "delete-merged";
      category = "[general commands]";
      help = "Delete merged branches";
      command = ''
        git fetch -p ; git branch -r | awk '{print $1}' | egrep -v -f - <(git branch -vv | grep origin) | awk '{print $1}' | xargs -r git branch -D
      '';
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
