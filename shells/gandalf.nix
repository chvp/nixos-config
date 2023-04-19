{ pkgs, inputs }: pkgs.devshell.mkShell {
  name = "Gandalf";
  imports = [ "${inputs.devshell}/extra/language/ruby.nix" ];
  commands = [
    {
      name = "refresh-deps";
      category = "general commands";
      help = "Install dependencies";
      command = ''
        bundle install
        bundle pristine
      '';
    }
    {
      name = "mysql";
      category = "general commands";
      help = "Start mysql (in docker container)";
      command = ''
        docker run --name gandalf-db -p 3306:3306 --rm -v gandalf-db-data:/var/lib/mysql -e MYSQL_ROOT_PASSWORD=gandalf mariadb:latest
      '';
    }
    {
      name = "redis";
      package = pkgs.redis;
    }
  ];
  packages = with pkgs; [
    imagemagick
    libyaml
  ];
  env = [
    { name = "DATABASE_URL"; value = "mysql2://root:gandalf@127.0.0.1:3306/gandalf"; }
    { name = "TEST_DATABASE_URL"; value = "mysql2://root:gandalf@127.0.0.1:3306/gandalf_test"; }
  ];
  serviceGroups.server.services = {
    rails = {
      name = "server";
      command = "rails s -p 3000";
    };
    mysql.command = "mysql";
    redis.command = "redis-server --port 6379";
    sidekiq.command = "bundle exec sidekiq";
  };
  language.ruby = {
    package = pkgs.ruby_3_0;
    nativeDeps = [ pkgs.libmysqlclient pkgs.zlib ];
  };
}
